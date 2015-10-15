package me.piebridge.prevent.framework;

import android.app.ActivityThread;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.os.Handler;
import android.os.HandlerThread;
import android.os.Process;
import android.text.TextUtils;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.common.GmsUtils;
import me.piebridge.prevent.common.PackageUtils;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.framework.util.ActivityRecordUtils;
import me.piebridge.prevent.framework.util.HideApiUtils;
import me.piebridge.prevent.framework.util.LogUtils;
import me.piebridge.prevent.framework.util.LogcatUtils;
import me.piebridge.prevent.ui.PreventProvider;

public final class SystemHook {

    public static final int TIME_SUICIDE = 6;
    public static final int TIME_DESTROY = 6;
    public static final int TIME_IMMEDIATE = 1;
    public static final int TIME_CHECK_SERVICE = 30;
    public static final int TIME_KILL = 1;
    public static final int TIME_CHECK_GMS = 30;
    public static final int TIME_CHECK_DISALLOW = 5;
    public static final int TIME_CHECK_USER_LEAVING = 60;
    public static final int FIRST_APPLICATION_UID = 10000;

    private static Context mContext;
    private static boolean activated;
    private static ClassLoader mClassLoader;
    private static Map<String, Boolean> mPreventPackages;

    private static Set<String> checkingPackageNames = new TreeSet<String>();
    private static Set<String> checkingWhiteList = new TreeSet<String>();
    private static Set<String> runningGapps = new TreeSet<String>();
    private static final Object CHECKING_LOCK = new Object();

    private static ScheduledThreadPoolExecutor singleExecutor = new ScheduledThreadPoolExecutor(0x2);

    private static ScheduledThreadPoolExecutor retrievingExecutor = new ScheduledThreadPoolExecutor(0x2);

    private static ScheduledThreadPoolExecutor checkingExecutor = new ScheduledThreadPoolExecutor(0x2);

    private static ScheduledThreadPoolExecutor forceStopExecutor = new ScheduledThreadPoolExecutor(0x1);

    private static ScheduledFuture<?> checkingFuture;
    private static ScheduledFuture<?> killingFuture;
    private static Map<String, ScheduledFuture<?>> serviceFutures = new HashMap<String, ScheduledFuture<?>>();

    private static RetrievingTask retrievingTask;
    private static Future<?> retrievingFuture;

    private static SystemReceiver systemReceiver;

    private static ScheduledThreadPoolExecutor restoreExecutor = new ScheduledThreadPoolExecutor(0x2);
    private static final Object RESTORE_LOCK = new Object();
    private static Map<String, ScheduledFuture<?>> restoreFutures = new HashMap<String, ScheduledFuture<?>>();
    private static boolean destroyProcesses;

    private SystemHook() {

    }

    public static void setClassLoader(ClassLoader classLoader) {
        mClassLoader = classLoader;
    }

    public static ClassLoader getClassLoader() {
        return mClassLoader;
    }

    private static Context getContext(Object activityManagerService) {
        Field field = null;
        Class<?> clazz = activityManagerService.getClass();
        while (clazz != null && field == null) {
            try {
                field = clazz.getDeclaredField("mContext");
            } catch (NoSuchFieldException e) { // NOSONAR
                PreventLog.d("cannot find mContext in " + clazz.getName());
                clazz = clazz.getSuperclass();
            }
        }

        if (field != null) {
            field.setAccessible(true);
            try {
                return (Context) field.get(activityManagerService);
            } catch (IllegalAccessException e) {
                PreventLog.d("cannot visit mContext in " + activityManagerService.getClass().getName(), e);
            }
        }
        return null;
    }

    public static Context getRegisterContext(Object activityManagerService) {
        Context context = ActivityThread.currentApplication();
        if (context != null && checkRegisterContext(context)) {
            return context;
        }
        context = getContext(activityManagerService);
        if (context != null && checkRegisterContext(context)) {
            return context;
        }
        return null;
    }

    private static boolean checkRegisterContext(Context context) {
        BroadcastReceiver receiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                // do nothing
            }
        };
        IntentFilter filter = new IntentFilter();
        filter.addAction(PreventIntent.ACTION_GET_PACKAGES);
        filter.addDataScheme(PreventIntent.SCHEME);
        try {
            context.registerReceiver(receiver, filter);
            context.unregisterReceiver(receiver);
            return true;
        } catch (SecurityException e) { // NOSONAR
            PreventLog.d("cannot register: " + e.getMessage());
            return false;
        }
    }

    public static boolean registerReceiver() {
        HandlerThread thread = new HandlerThread("PreventService");
        thread.start();
        Handler handler = new Handler(thread.getLooper());

        systemReceiver = new SystemReceiver(mContext, mPreventPackages);

        IntentFilter manager = new IntentFilter();
        for (String action : SystemReceiver.MANAGER_ACTIONS) {
            manager.addAction(action);
        }
        manager.addDataScheme(PreventIntent.SCHEME);
        mContext.registerReceiver(systemReceiver, manager, PreventIntent.PERMISSION_MANAGER, handler);

        IntentFilter filter = new IntentFilter();
        for (String action : SystemReceiver.PACKAGE_ACTIONS) {
            filter.addAction(action);
        }
        filter.addDataScheme("package");
        mContext.registerReceiver(systemReceiver, filter, null, handler);

        IntentFilter noSchemeFilter = new IntentFilter();
        for (String action : SystemReceiver.NON_SCHEME_ACTIONS) {
            noSchemeFilter.addAction(action);
        }
        noSchemeFilter.setPriority(IntentFilter.SYSTEM_HIGH_PRIORITY);
        mContext.registerReceiver(systemReceiver, noSchemeFilter, null, handler);

        Intent intent = new Intent(PreventIntent.ACTION_REGISTERED);
        intent.setPackage(BuildConfig.APPLICATION_ID);
        mContext.sendBroadcast(intent, PreventIntent.PERMISSION_MANAGER);
        PreventLog.i("registered receiver");
        return true;
    }

    public static boolean retrievePreventsIfNeeded(final Object activityManagerService) {
        if (mPreventPackages != null) {
            return true;
        }
        if (mContext == null && (mContext = getRegisterContext(activityManagerService)) == null) {
            return false;
        }
        PreventLog.d("context: " + mContext.getClass().getName());
        if (retrievingTask == null) {
            retrievingTask = new RetrievingTask();
            retrievingFuture = retrievingExecutor.submit(retrievingTask);
            retrievingExecutor.schedule(new Runnable() {
                @Override
                public void run() {
                    if (mPreventPackages == null) {
                        PreventLog.d("checking prevents, no data");
                        if (!retrievingFuture.isDone()) {
                            retrievingFuture.cancel(true);
                        }
                        PreventLog.d("checking prevents, wait for next check");
                        retrievingTask = null;
                    } else {
                        PreventLog.d("checking prevents, ok");
                    }
                }
            }, 0x5, TimeUnit.SECONDS);
        }
        return true;
    }

    public static boolean isSystemHook() {
        return Process.myUid() == Process.SYSTEM_UID;
    }

    static String getProcessName(int pid) {
        File file = new File(new File("/proc", String.valueOf(pid)), "cmdline");
        return getContent(file);
    }

    private static String getContent(File file) {
        if (!file.isFile() || !file.canRead()) {
            return null;
        }

        try {
            InputStream is = new BufferedInputStream(new FileInputStream(file));
            ByteArrayOutputStream os = new ByteArrayOutputStream();
            int length;
            byte[] buffer = new byte[0x1000];
            while ((length = is.read(buffer)) != -1) {
                os.write(buffer, 0, length);
            }
            return os.toString().trim();
        } catch (IOException e) {
            PreventLog.e("cannot read file " + file, e);
            return null;
        }
    }

    public static void checkRunningServices(final String packageName, final boolean forcestop) {
        if (packageName == null) {
            return;
        }
        ScheduledFuture<?> serviceFuture;
        synchronized (CHECKING_LOCK) {
            serviceFuture = serviceFutures.get(packageName);
            if (serviceFuture != null && serviceFuture.getDelay(TimeUnit.SECONDS) > 0) {
                GmsUtils.decreaseGmsCount(mContext, packageName);
                serviceFuture.cancel(false);
            }
            if (!GmsUtils.isGms(packageName)) {
                checkingWhiteList.add(packageName);
            }
        }
        GmsUtils.increaseGmsCount(mContext, packageName);
        serviceFuture = checkingExecutor.schedule(new CheckingRunningService(mContext, mPreventPackages) {
            @Override
            protected Collection<String> preparePackageNames() {
                return Collections.singletonList(packageName);
            }

            @Override
            protected Collection<String> prepareWhiteList() {
                return prepareServiceWhiteList(packageName, forcestop);
            }
        }, GmsUtils.isGms(packageName) ? TIME_CHECK_GMS : TIME_CHECK_SERVICE, TimeUnit.SECONDS);
        synchronized (CHECKING_LOCK) {
            serviceFutures.put(packageName, serviceFuture);
        }
    }

    private static Collection<String> prepareServiceWhiteList(String packageName, boolean forcestop) {
        GmsUtils.decreaseGmsCount(mContext, packageName);
        if (!GmsUtils.isGms(packageName)) {
            synchronized (CHECKING_LOCK) {
                checkingWhiteList.remove(packageName);
            }
        }
        if (!GmsUtils.canStopGms()) {
            return GmsUtils.getGmsPackages();
        } else {
            if (forcestop) {
                HideApiUtils.forceStopPackage(mContext, packageName);
            }
            return Collections.emptyList();
        }
    }

    public static void cancelCheck(String packageName) {
        if (packageName != null) {
            synchronized (CHECKING_LOCK) {
                checkingPackageNames.remove(packageName);
            }
        }
    }

    public static boolean checkRunningServices(final String packageName, int seconds) {
        if (packageName != null) {
            synchronized (CHECKING_LOCK) {
                checkingPackageNames.add(packageName);
            }
        }
        if (checkingFuture != null && checkingFuture.getDelay(TimeUnit.SECONDS) > 0) {
            return false;
        }
        checkingFuture = singleExecutor.schedule(new CheckingRunningService(mContext, mPreventPackages) {
            @Override
            protected Collection<String> preparePackageNames() {
                return prepareCheckingPackageNames();
            }

            @Override
            protected Collection<String> prepareWhiteList() {
                return prepareCheckingWhiteList();
            }
        }, seconds, TimeUnit.SECONDS);
        return true;
    }

    private static Collection<String> prepareCheckingPackageNames() {
        Set<String> packageNames = new TreeSet<String>();
        synchronized (CHECKING_LOCK) {
            packageNames.addAll(checkingPackageNames);
            checkingPackageNames.clear();
        }
        return packageNames;
    }

    private static Collection<String> prepareCheckingWhiteList() {
        Set<String> whiteList = new TreeSet<String>();
        synchronized (CHECKING_LOCK) {
            whiteList.addAll(checkingWhiteList);
        }
        if (!GmsUtils.canStopGms()) {
            whiteList.addAll(GmsUtils.getGmsPackages());
        }
        return whiteList;
    }

    static void forceStopPackageForce(final String packageName, int second) {
        singleExecutor.schedule(new Runnable() {
            @Override
            public void run() {
                forceStopPackage(packageName);
            }
        }, second, TimeUnit.SECONDS);
    }

    public static void forceStopPackage(final String packageName) {
        if (!Boolean.TRUE.equals(mPreventPackages.get(packageName))) {
            return;
        }
        forceStopExecutor.execute(new Runnable() {
            @Override
            public void run() {
                if (Boolean.TRUE.equals(mPreventPackages.get(packageName))) {
                    HideApiUtils.forceStopPackage(mContext, packageName);
                }
            }
        });
    }

    public static boolean killNoFather() {
        if (killingFuture != null && killingFuture.getDelay(TimeUnit.SECONDS) > 0) {
            return false;
        }
        killingFuture = singleExecutor.schedule(new Runnable() {
            @Override
            public void run() {
                try {
                    doKillNoFather();
                } catch (Throwable t) { // NOSONAR
                    PreventLog.e("cannot killNoFather", t);
                }
            }
        }, TIME_KILL, TimeUnit.SECONDS);
        return true;
    }

    private static void doKillNoFather() {
        File proc = new File("/proc");
        for (File file : proc.listFiles()) {
            if (file.isDirectory() && TextUtils.isDigitsOnly(file.getName())) {
                int pid = Integer.parseInt(file.getName());
                int uid = HideApiUtils.getUidForPid(pid);
                if (HideApiUtils.getParentPid(pid) == 1 && uid >= FIRST_APPLICATION_UID) {
                    killIfNeed(uid, pid);
                }
            }
        }
    }

    private static void killIfNeed(int uid, int pid) {
        String[] names = mContext.getPackageManager().getPackagesForUid(uid);
        if (names == null || isPrevent(names)) {
            Process.killProcess(pid);
            String name;
            if (names == null) {
                name = "(uid: " + uid + ", process: + " + getProcessName(pid) + ")";
            } else if (names.length == 1) {
                name = names[0];
            } else {
                name = Arrays.asList(names).toString();
            }
            LogUtils.logKill(pid, "without parent", name);
        }
    }

    private static boolean isPrevent(String[] names) {
        for (String name : names) {
            if (mPreventPackages.containsKey(name)) {
                return true;
            }
        }
        return false;
    }

    public static void restorePrevent(String packageName) {
        if (systemReceiver != null && 0 == systemReceiver.countCounter(packageName) && Boolean.FALSE.equals(mPreventPackages.get(packageName))) {
            PreventLog.v("restore prevent for " + packageName);
            mPreventPackages.put(packageName, true);
            checkRunningServices(packageName, TIME_DESTROY);
        }
    }

    public static void onStartActivity(Object activityRecord) {
        if (systemReceiver != null) {
            systemReceiver.onStartActivity(activityRecord);
        }
    }

    public static void onDestroyActivity(Object activityRecord) {
        if (systemReceiver != null) {
            systemReceiver.onDestroyActivity(activityRecord);
        }
    }

    public static void onResumeActivity(Object activityRecord) {
        if (systemReceiver != null) {
            systemReceiver.onResumeActivity(activityRecord);
        }
    }

    public static void onUserLeavingActivity(Object activityRecord) {
        if (systemReceiver != null) {
            systemReceiver.onUserLeavingActivity(activityRecord);
        }
    }

    public static void onStartHomeActivity(String packageName) {
        if (systemReceiver != null) {
            systemReceiver.onDestroyActivity("start home activity", packageName);
        }
    }

    public static void onMoveActivityToBack(Object activityRecord) {
        if (systemReceiver != null) {
            String packageName = ActivityRecordUtils.getPackageName(activityRecord);
            systemReceiver.onDestroyActivity("move activity to back", packageName);
        }
    }

    public static void onAppDied(Object processRecord) {
        if (systemReceiver != null) {
            systemReceiver.onAppDied(processRecord);
        }
    }

    public static boolean hasRunningActivity(String packageName) {
        if (packageName != null && systemReceiver != null && systemReceiver.countCounter(packageName) != 0) {
            return true;
        }

        // for temp allow
        ScheduledFuture<?> restoreFuture = restoreFutures.get(packageName);
        return restoreFuture != null && restoreFuture.getDelay(TimeUnit.SECONDS) > 0;
    }

    public static boolean cannotPrevent(String packageName) {
        return packageName != null && isFramework(packageName);
    }

    public static boolean isFramework(String packageName) {
        return "android".equals(packageName);
    }

    public static boolean isSystemPackage(String packageName) {
        if (packageName == null) {
            return false;
        }
        if (isFramework(packageName) || GmsUtils.isGms(packageName)) {
            return true;
        }
        try {
            PackageManager pm = mContext.getPackageManager();
            int flags = pm.getApplicationInfo(packageName, 0).flags;
            return PackageUtils.isSystemPackage(flags);
        } catch (PackageManager.NameNotFoundException e) {
            PreventLog.d("cannot find package: " + packageName, e);
            return false;
        }
    }

    public static boolean isActivated() {
        return activated;
    }

    public static void setDestroyProcesses(boolean destroyProcesses) {
        SystemHook.destroyProcesses = destroyProcesses;
    }

    public static boolean isDestroyProcesses() {
        return destroyProcesses;
    }

    private static class RetrievingTask implements Runnable {
        @Override
        public void run() {
            PreventLog.d("RetrievingTask");

            Map<String, Boolean> preventPackages = new HashMap<String, Boolean>();
            loadPrevent(preventPackages);
            PreventLog.d("prevents: " + preventPackages.size());
            mPreventPackages = new ConcurrentHashMap<String, Boolean>();
            mPreventPackages.putAll(preventPackages);

            registerReceiver();
            ActivityManagerServiceHook.setContext(mContext, mPreventPackages);
            IntentFilterHook.setContext(mContext, mPreventPackages);
            PreventLog.i("prevent running " + BuildConfig.VERSION_NAME + " activated");
            activated = true;

            LogcatUtils.logcat(mContext, "boot");
        }

        private void loadPrevent(Map<String, Boolean> preventPackages) {
            Cursor cursor = mContext.getContentResolver().query(PreventProvider.CONTENT_URI, null, null, null, null);
            int index = cursor.getColumnIndex(PreventProvider.COLUMN_PACKAGE);
            while (cursor.moveToNext()) {
                String name = cursor.getString(index);
                if (name != null && !preventPackages.containsKey(name)) {
                    preventPackages.put(name, true);
                }
            }
            cursor.close();
        }
    }

    public static void updateRunningGapps(String packageName, boolean added) {
        if (mContext == null || packageName == null) {
            return;
        }
        systemReceiver.removeLeavingPackage(packageName);
        PackageManager pm = mContext.getPackageManager();
        if (GmsUtils.isGapps(pm, packageName) && pm.getLaunchIntentForPackage(packageName) != null) {
            if (added) {
                if (!runningGapps.contains(packageName)) {
                    PreventLog.d("add " + packageName + " to running gapps: " + runningGapps);
                }
                runningGapps.add(packageName);
            } else {
                if (runningGapps.contains(packageName)) {
                    PreventLog.d("remove " + packageName + " from running gapps: " + runningGapps);
                    checkRunningServices(null, SystemHook.TIME_CHECK_SERVICE);
                }
                runningGapps.remove(packageName);
            }
        }
    }

    public static boolean hasRunningGapps() {
        Iterator<String> it = runningGapps.iterator();
        while (it.hasNext()) {
            String packageName = it.next();
            int count = systemReceiver.countCounter(packageName);
            if (count == 0) {
                it.remove();
            }
        }
        if (!runningGapps.isEmpty()) {
            PreventLog.d("running gapps: " + runningGapps);
            return true;
        } else {
            return false;
        }
    }

    public static void restoreLater(final String packageName) {
        synchronized (RESTORE_LOCK) {
            ScheduledFuture<?> restoreFuture = restoreFutures.get(packageName);
            if (restoreFuture != null && restoreFuture.getDelay(TimeUnit.SECONDS) > 0) {
                restoreFuture.cancel(false);
            }
            restoreFuture = restoreExecutor.schedule(new Runnable() {
                @Override
                public void run() {
                    restorePrevent(packageName);
                }
            }, TIME_CHECK_DISALLOW, TimeUnit.SECONDS);
            restoreFutures.put(packageName, restoreFuture);
        }
    }

}