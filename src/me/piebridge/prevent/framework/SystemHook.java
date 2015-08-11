package me.piebridge.prevent.framework;

import android.app.ActivityManager;
import android.app.ActivityThread;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
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
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import me.piebridge.prevent.common.GmsUtils;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.framework.util.HideApiUtils;
import me.piebridge.prevent.framework.util.LogUtils;
import me.piebridge.prevent.framework.util.LogcatUtils;
import me.piebridge.prevent.ui.PreventProvider;

public final class SystemHook {

    public static final int TIME_SUICIDE = 6;
    public static final int TIME_DESTROY = 6;
    public static final int TIME_PREVENT = 30;
    public static final int TIME_IMMEDIATE = 1;
    public static final int TIME_CHECK_SERVICE = 30;
    public static final int TIME_KILL = 1;
    public static final int FIRST_APPLICATION_UID = 10000;

    private static Context mContext;
    private static ClassLoader mClassLoader;
    private static Map<String, Boolean> mPreventPackages;

    private static Set<String> checkingPackageNames = new TreeSet<String>();
    private static Set<String> checkingWhiteList = new TreeSet<String>();
    private static final Object CHECKING_LOCK = new Object();

    private static ScheduledThreadPoolExecutor singleExecutor = new ScheduledThreadPoolExecutor(0x2);

    private static ScheduledThreadPoolExecutor checkingExecutor = new ScheduledThreadPoolExecutor(0x2);

    private static ScheduledThreadPoolExecutor forceStopExecutor = new ScheduledThreadPoolExecutor(0x1);

    private static ScheduledFuture<?> checkingFuture;
    private static ScheduledFuture<?> killingFuture;
    private static Map<String, ScheduledFuture<?>> serviceFutures = new HashMap<String, ScheduledFuture<?>>();

    private static RetrievingTask retrievingTask;

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

        BroadcastReceiver receiver = new SystemReceiver(mPreventPackages);

        IntentFilter manager = new IntentFilter();
        manager.addAction(PreventIntent.ACTION_GET_PACKAGES);
        manager.addAction(PreventIntent.ACTION_GET_PROCESSES);
        manager.addAction(PreventIntent.ACTION_UPDATE_PREVENT);
        manager.addDataScheme(PreventIntent.SCHEME);
        mContext.registerReceiver(receiver, manager, PreventIntent.PERMISSION_MANAGER, handler);

        IntentFilter hook = new IntentFilter();
        hook.addAction(PreventIntent.ACTION_INCREASE_COUNTER);
        hook.addAction(PreventIntent.ACTION_DECREASE_COUNTER);
        hook.addAction(PreventIntent.ACTION_RESTART);
        hook.addAction(PreventIntent.ACTION_ACTIVITY_DESTROY);
        hook.addAction(PreventIntent.ACTION_FORCE_STOP);
        hook.addDataScheme(PreventIntent.SCHEME);
        // FIXME: check permission
        mContext.registerReceiver(receiver, hook, null, handler);

        IntentFilter filter = new IntentFilter();
        filter.addAction(Intent.ACTION_PACKAGE_RESTARTED);
        filter.addAction(Intent.ACTION_PACKAGE_ADDED);
        filter.addDataScheme("package");
        mContext.registerReceiver(receiver, filter, null, handler);

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
            singleExecutor.submit(retrievingTask);
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

    public static void checkRunningServices(final String packageName) {
        if (packageName == null) {
            return;
        }
        ScheduledFuture<?> serviceFuture;
        synchronized (CHECKING_LOCK) {
            serviceFuture = serviceFutures.get(packageName);
            if (serviceFuture != null && checkingFuture.getDelay(TimeUnit.SECONDS) > 0) {
                return;
            }
            checkingWhiteList.add(packageName);
        }
        GmsUtils.increaseGmsCount(mContext, packageName);
        serviceFuture = checkingExecutor.schedule(new CheckingRunningService(mContext, mPreventPackages) {
            @Override
            protected Collection<String> preparePackageNames() {
                return Collections.singletonList(packageName);
            }

            @Override
            protected Collection<String> prepareWhiteList() {
                return prepareServiceWhiteList(packageName);
            }
        }, GmsUtils.isDependency(mContext, packageName) ? TIME_CHECK_SERVICE + TIME_PREVENT : TIME_CHECK_SERVICE, TimeUnit.SECONDS);
        synchronized (CHECKING_LOCK) {
            serviceFutures.put(packageName, serviceFuture);
        }
    }

    private static Collection<String> prepareServiceWhiteList(String packageName) {
        int gmsCount = GmsUtils.decreaseGmsCount(mContext, packageName);
        if (!GmsUtils.GMS.equals(packageName) || gmsCount == 0) {
            synchronized (CHECKING_LOCK) {
                checkingWhiteList.remove(packageName);
            }
        }
        if (gmsCount > 0) {
            return Collections.singletonList(GmsUtils.GMS);
        } else {
            return Collections.emptyList();
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

    public static void forceStopPackageLater(final String packageName, int second) {
        singleExecutor.schedule(new Runnable() {
            @Override
            public void run() {
                forceStopPackage(packageName);
            }
        }, second, TimeUnit.SECONDS);
    }

    public static void forceStopPackageLaterIfPrevent(final String packageName, int second) {
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
                    ActivityManager activityManager = (ActivityManager) mContext.getSystemService(Context.ACTIVITY_SERVICE);
                    HideApiUtils.forceStopPackage(activityManager, packageName);
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

    private static class RetrievingTask implements Runnable {
        @Override
        public void run() {
            PreventLog.d("RetrievingTask");
            Cursor cursor = mContext.getContentResolver().query(PreventProvider.CONTENT_URI, null, null, null, null);
            Map<String, Boolean> preventPackages = new HashMap<String, Boolean>();
            int index = cursor.getColumnIndex(PreventProvider.COLUMN_PACKAGE);
            while (cursor.moveToNext()) {
                String name = cursor.getString(index);
                if (name != null && !preventPackages.containsKey(name)) {
                    preventPackages.put(name, Boolean.TRUE);
                }
            }
            cursor.close();
            PreventLog.d("prevents: " + preventPackages.size());
            mPreventPackages = new ConcurrentHashMap<String, Boolean>();
            mPreventPackages.putAll(preventPackages);

            registerReceiver();
            ActivityManagerServiceHook.setContext(mContext, mPreventPackages);
            IntentFilterHook.setContext(mContext, mPreventPackages);
            PreventLog.d("prevent running activated");

            LogcatUtils.logcat(mContext);
        }
    }

}