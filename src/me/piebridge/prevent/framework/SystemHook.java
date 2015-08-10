package me.piebridge.prevent.framework;

import android.accounts.AccountManager;
import android.app.ActivityManager;
import android.app.ActivityThread;
import android.appwidget.AppWidgetManager;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageParser;
import android.database.Cursor;
import android.net.Uri;
import android.os.Binder;
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
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.common.GmsUtils;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.framework.util.BroadcastFilterUtils;
import me.piebridge.prevent.framework.util.HideApiUtils;
import me.piebridge.prevent.framework.util.LogUtils;
import me.piebridge.prevent.framework.util.NotificationManagerServiceUtils;
import me.piebridge.prevent.framework.util.TaskRecordUtils;
import me.piebridge.prevent.framework.util.WidgetUtils;
import me.piebridge.prevent.ui.PreventProvider;

public final class SystemHook {

    private static boolean registered = false;
    private static boolean gotprevent = false;

    static final int TIME_SUICIDE = 6;
    static final int TIME_DESTROY = 6;
    static final int TIME_PREVENT = 30;
    static final int TIME_IMMEDIATE = 1;
    static final int TIME_CHECK_SERVICE = 30;

    static final int TIME_KILL = 1;

    static final int FIRST_APPLICATION_UID = 10000;

    private static Context mContext;

    static Map<String, Boolean> preventPackages = new ConcurrentHashMap<String, Boolean>();

    static Map<String, Integer> packageUids = new HashMap<String, Integer>();

    static Map<String, Map<Integer, AtomicInteger>> packageCounters = new ConcurrentHashMap<String, Map<Integer, AtomicInteger>>();

    static Set<String> checkingPackageNames = new TreeSet<String>();
    static Set<String> checkingWhiteList = new TreeSet<String>();
    static final Object CHECKING_LOCK = new Object();

    private static ScheduledThreadPoolExecutor singleExecutor = new ScheduledThreadPoolExecutor(0x2);

    private static ScheduledThreadPoolExecutor checkingExecutor = new ScheduledThreadPoolExecutor(0x2);

    private static ScheduledThreadPoolExecutor forceStopExecutor = new ScheduledThreadPoolExecutor(0x1);

    private static ClassLoader classLoader;

    private static Handler mHandler;

    private static ScheduledFuture<?> checkingFuture;
    private static ScheduledFuture<?> killingFuture;
    private static Map<String, ScheduledFuture<?>> serviceFutures = new HashMap<String, ScheduledFuture<?>>();

    private static Set<String> SAFE_BROADCAST_ACTIONS = new HashSet<String>(Arrays.asList(
            // http://developer.android.com/guide/topics/appwidgets/index.html#Manifest
            // http://developer.android.com/reference/android/appwidget/AppWidgetManager.html#ACTION_APPWIDGET_UPDATE
            AppWidgetManager.ACTION_APPWIDGET_UPDATE,
            Intent.ACTION_CONFIGURATION_CHANGED
    ));

    private static AccountWatcher accountWatcher;

    private SystemHook() {

    }

    public static void setClassLoader(ClassLoader classLoader) {
        SystemHook.classLoader = classLoader;
    }

    public static ClassLoader getClassLoader() {
        return SystemHook.classLoader;
    }

    public static Map<String, Boolean> getPreventPackages() {
        return preventPackages;
    }

    public static Map<String, Map<Integer, AtomicInteger>> getPackageCounters() {
        return packageCounters;
    }

    public static Map<String, Integer> getPackageUids() {
        return packageUids;
    }

    public static IntentFilterMatchResult hookIntentFilter$match(Object filter, Object[] args) { // NOSONAR
        if (mContext == null) {
            return IntentFilterMatchResult.NONE;
        }

        if (!isSystemHook()) {
            return IntentFilterMatchResult.NONE;
        }

        String action = (String) args[0x0];

        if (Intent.ACTION_PACKAGE_RESTARTED.equals(action) && NotificationManagerServiceUtils.canHook(filter)) {
            return NotificationManagerServiceUtils.hook((Uri) args[0x3], preventPackages);
        }

        if (BuildConfig.DEBUG) {
            LogUtils.logIntentFilter(action, filter, null);
        }

        if (filter instanceof PackageParser.ActivityIntentInfo) {
            // for receiver, we don't block for activity
            @SuppressWarnings("unchecked")
            PackageParser.Activity activity = ((PackageParser.ActivityIntentInfo) filter).activity;
            PackageParser.Package owner = activity.owner;
            ApplicationInfo ai = owner.applicationInfo;
            if (canUseGms(ai)) {
                return IntentFilterMatchResult.NONE;
            }
            String packageName = ai.packageName;
            if (Boolean.TRUE.equals(preventPackages.get(packageName)) && owner.receivers.contains(activity)) {
                if (SAFE_BROADCAST_ACTIONS.contains(action)) {
                    return IntentFilterMatchResult.NONE;
                }
                if (BuildConfig.DEBUG) {
                    LogUtils.logIntentFilter(true, filter, action, packageName);
                }
                return IntentFilterMatchResult.NO_MATCH;
            }
        } else if (filter instanceof PackageParser.ServiceIntentInfo) {
            @SuppressWarnings("unchecked")
            PackageParser.Service service = ((PackageParser.ServiceIntentInfo) filter).service;
            PackageParser.Package owner = service.owner;
            ApplicationInfo ai = owner.applicationInfo;
            if (canUseGms(ai)) {
                return IntentFilterMatchResult.NONE;
            }
            String packageName = ai.packageName;
            boolean prevents = Boolean.TRUE.equals(preventPackages.get(packageName));
            if (!prevents || (AccountManager.ACTION_AUTHENTICATOR_INTENT.equals(action) && accountWatcher.containsPackage(packageName))) {
                return IntentFilterMatchResult.NONE;
            }
            if (Binder.getCallingUid() != Process.SYSTEM_UID) {
                if (BuildConfig.DEBUG) {
                    LogUtils.logIntentFilter(true, filter, action, packageName);
                }
                return IntentFilterMatchResult.NO_MATCH;
            } else {
                LogUtils.logIntentFilter(false, filter, action, packageName);
            }
        } else if (Intent.ACTION_CLOSE_SYSTEM_DIALOGS.equals(action)) {
            // for dynamic broadcast, we only disable ACTION_CLOSE_SYSTEM_DIALOGS
            String packageName = BroadcastFilterUtils.getPackageName(filter);
            if (packageName == null) {
                return IntentFilterMatchResult.NONE;
            }
            if (preventPackages.containsKey(packageName)) {
                LogUtils.logIntentFilter(true, filter, action, packageName);
                return IntentFilterMatchResult.NO_MATCH;
            }
            if (BuildConfig.DEBUG) {
                LogUtils.logIntentFilter(action, filter, packageName);
            }
        }

        return IntentFilterMatchResult.NONE;
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

    public static boolean registerReceiversIfNeeded(Object activityManagerService) {
        if (registered) {
            return true;
        }

        if (mHandler == null) {
            HandlerThread thread = new HandlerThread("PreventService");
            thread.start();
            mHandler = new Handler(thread.getLooper());
        }

        Context context = ActivityThread.currentApplication();
        if (context != null && doRegisterReceivers(context)) {
            return true;
        } else {
            // fallback
            context = getContext(activityManagerService);
            return context != null && doRegisterReceivers(context);
        }
    }

    private static boolean doRegisterReceivers(Context context) {
        BroadcastReceiver receiver = new SystemReceiver(context);

        IntentFilter manager = new IntentFilter();
        manager.addAction(PreventIntent.ACTION_GET_PACKAGES);
        manager.addAction(PreventIntent.ACTION_GET_PROCESSES);
        manager.addAction(PreventIntent.ACTION_UPDATE_PREVENT);
        manager.addDataScheme(PreventIntent.SCHEME);

        try {
            context.registerReceiver(receiver, manager, PreventIntent.PERMISSION_MANAGER, mHandler);
        } catch (SecurityException e) { // NOSONAR
            PreventLog.d("cannot register: " + e.getMessage());
            return false;
        }

        IntentFilter hook = new IntentFilter();
        hook.addAction(PreventIntent.ACTION_INCREASE_COUNTER);
        hook.addAction(PreventIntent.ACTION_DECREASE_COUNTER);
        hook.addAction(PreventIntent.ACTION_RESTART);
        hook.addAction(PreventIntent.ACTION_ACTIVITY_DESTROY);
        hook.addAction(PreventIntent.ACTION_FORCE_STOP);
        hook.addDataScheme(PreventIntent.SCHEME);
        // FIXME: check permission
        context.registerReceiver(receiver, hook, null, mHandler);

        IntentFilter filter = new IntentFilter();
        filter.addAction(Intent.ACTION_PACKAGE_RESTARTED);
        filter.addAction(Intent.ACTION_PACKAGE_ADDED);
        filter.addDataScheme("package");
        context.registerReceiver(new PackageReceiver(), filter, null, mHandler);

        registered = true;
        PreventLog.i("registered receiver");

        mContext = context;

        accountWatcher = AccountWatcher.get(mContext, mHandler);
        return true;
    }

    private static boolean retrievePreventsIfNeeded() {
        // this is for android 5.X, selinux deny the read file for app
        if (!preventPackages.isEmpty()) {
            return true;
        }
        if (mContext == null) {
            return false;
        }
        if (gotprevent) {
            return true;
        }
        doRetrievePrevents();
        gotprevent = true;
        return true;
    }

    private static void doRetrievePrevents() {
        singleExecutor.execute(new Runnable() {
            @Override
            public void run() {
                Cursor cursor = mContext.getContentResolver().query(PreventProvider.CONTENT_URI, null, null, null, null);
                int index = cursor.getColumnIndex(PreventProvider.COLUMN_PACKAGE);
                while (cursor.moveToNext()) {
                    String name = cursor.getString(index);
                    if (name != null && !preventPackages.containsKey(name)) {
                        preventPackages.put(name, Boolean.TRUE);
                    }
                }
                PreventLog.d("prevents: " + preventPackages.size());
            }
        });
    }

    private static boolean isWantedStartProcessLocked(Class<?>[] types) {
        if (types == null || types.length < 0x6) {
            return false;
        }
        return ApplicationInfo.class.equals(types[0x1])
                && int.class.equals(types[0x3])
                && String.class.equals(types[0x4])
                && ComponentName.class.equals(types[0x5]);
    }

    public static Method getStartProcessLocked(Class<?> ActivityManagerService) { // NOSONAR
        Method startProcessLocked = null;
        for (Method method : ActivityManagerService.getDeclaredMethods()) {
            if (!"startProcessLocked".equals(method.getName()) || !"ProcessRecord".equals(method.getReturnType().getSimpleName()) || !isWantedStartProcessLocked(method.getParameterTypes())) {
                continue;
            }
            if (startProcessLocked == null || startProcessLocked.getParameterTypes().length < method.getParameterTypes().length) {
                startProcessLocked = method;
            }
        }
        return startProcessLocked;
    }

    public static Method getCleanUpRemovedTaskLocked(Class<?> activityManagerService) {
        for (Method method : activityManagerService.getDeclaredMethods()) {
            if ("cleanUpRemovedTaskLocked".equals(method.getName()) && method.getParameterTypes().length == 0x2) {
                return method;
            }
        }
        return null;
    }

    private static boolean canUseGms(int callingUid, int uid) {
        PackageManager pm = mContext.getPackageManager();
        String[] packageNames = pm.getPackagesForUid(callingUid);
        if (packageNames == null || packageNames.length == 0) {
            return false;
        }
        if (pm.checkSignatures(callingUid, uid) == PackageManager.SIGNATURE_MATCH) {
            PreventLog.v("allow " + packageNames[0] + "(same signature) with gms to use gms if needed");
            return true;
        } else {
            return packageNames.length == 1 && canUseGms(pm, packageNames[0]);
        }
    }

    private static boolean canUseGms(PackageManager pm, String packageName) {
        if (pm.getLaunchIntentForPackage(packageName) == null || !packageName.startsWith(GmsUtils.GAPPS_PREFIX)) {
            return false;
        } else {
            PreventLog.v("allow " + packageName + " to use gms if needed");
            return true;
        }
    }

    private static boolean canUseGms(ApplicationInfo info) {
        if (!preventPackages.containsKey(GmsUtils.GMS)) {
            return true;
        }
        int uid = info.uid;
        int callingUid = Binder.getCallingUid();
        return callingUid == uid || (GmsUtils.GMS.equals(info.packageName) && canUseGms(callingUid, info.uid));
    }

    public static boolean beforeActivityManagerService$startProcessLocked(Object thiz, Object[] args) { // NOSONAR
        if (!isSystemHook()) {
            return true;
        }

        registerReceiversIfNeeded(thiz);

        ApplicationInfo info = (ApplicationInfo) args[0x1];
        String hostingType = (String) args[0x4];
        ComponentName hostingName = (ComponentName) args[0x5];
        String packageName = info.packageName;

        if ("content provider".equals(hostingType)) {
            retrievePreventsIfNeeded();
        }

        if (BuildConfig.DEBUG) {
            PreventLog.v("startProcessLocked, type: " + hostingType + ", name: " + hostingName + ", info: " + info);
        }

        Boolean prevents = preventPackages.get(packageName);
        // never block activity
        if ("activity".equals(hostingType) && Boolean.TRUE.equals(prevents)) {
            preventPackages.put(packageName, Boolean.FALSE);
            prevents = false;
        }

        if (!Boolean.TRUE.equals(prevents)) {
            return true;
        }

        // always block broadcast
        if ("broadcast".equals(hostingType)) {
            if (WidgetUtils.isWidget(mContext, hostingName)) {
                checkRunningServices(packageName);
                LogUtils.logStartProcess(false, packageName, hostingType + "(widget)", hostingName);
                return true;
            } else {
                checkRunningServices(packageName, TIME_PREVENT < TIME_DESTROY ? TIME_DESTROY : TIME_PREVENT);
                forceStopPackageLaterIfPrevent(packageName, TIME_PREVENT);
                LogUtils.logStartProcess(true, packageName, hostingType, hostingName);
                return false;
            }
        }

        // auto turn off service
        if ("service".equals(hostingType)) {
            checkRunningServices(packageName);
            LogUtils.logStartProcess(false, packageName, hostingType, hostingName);
        }

        return true;
    }

    public static void afterActivityManagerService$cleanUpRemovedTaskLocked(Object[] args) { // NOSONAR
        String packageName = TaskRecordUtils.getPackageName(args[0]);
        if (packageName != null && preventPackages.containsKey(packageName)) {
            autoPrevents(packageName);
        }
    }

    private static void autoPrevents(final String packageName) {
        singleExecutor.execute(new Runnable() {
            @Override
            public void run() {
                LogUtils.logForceStop("removeTask", packageName, "force in " + TIME_IMMEDIATE + "s");
                forceStopPackageForce(packageName, TIME_IMMEDIATE);
                if (preventPackages.containsKey(packageName)) {
                    preventPackages.put(packageName, Boolean.TRUE);
                }
            }
        });
    }

    private static boolean isSystemHook() {
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
            try {
                int length;
                byte[] buffer = new byte[0x1000];
                while ((length = is.read(buffer)) != -1) {
                    os.write(buffer, 0, length);
                }
            } finally {
                is.close();
            }
            return os.toString().trim();
        } catch (IOException e) {
            PreventLog.e("cannot read file " + file, e);
            return null;
        }
    }

    private static void checkRunningServices(final String packageName) {
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
        serviceFuture = checkingExecutor.schedule(new CheckingRunningService(mContext) {
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

    static boolean checkRunningServices(final String packageName, int seconds) {
        if (mContext == null) {
            PreventLog.e("context is null, cannot check running services for " + packageName);
            return false;
        }
        if (packageName != null) {
            synchronized (CHECKING_LOCK) {
                checkingPackageNames.add(packageName);
            }
        }
        if (checkingFuture != null && checkingFuture.getDelay(TimeUnit.SECONDS) > 0) {
            return false;
        }
        checkingFuture = singleExecutor.schedule(new CheckingRunningService(mContext) {
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
        synchronized (SystemHook.CHECKING_LOCK) {
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
                if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
                    forceStopPackage(packageName);
                }
            }
        }, second, TimeUnit.SECONDS);
    }

    static void forceStopPackageLater(final String packageName, int second) {
        singleExecutor.schedule(new Runnable() {
            @Override
            public void run() {
                if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
                    forceStopPackage(packageName);
                }
            }
        }, second, TimeUnit.SECONDS);
    }

    private static void forceStopPackageLaterIfPrevent(final String packageName, int second) {
        singleExecutor.schedule(new Runnable() {
            @Override
            public void run() {
                if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
                    forceStopPackage(packageName);
                }
            }
        }, second, TimeUnit.SECONDS);
    }

    static void forceStopPackage(final String packageName) {
        if (!Boolean.TRUE.equals(preventPackages.get(packageName))) {
            return;
        }
        if (mContext == null) {
            PreventLog.e("context is null, cannot force stop package" + packageName);
            return;
        }
        try {
            forceStopExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    ActivityManager activityManager = (ActivityManager) mContext.getSystemService(Context.ACTIVITY_SERVICE);
                    HideApiUtils.forceStopPackage(activityManager, packageName);
                }
            });
        } catch (Throwable t) { // NOSONAR
            PreventLog.e("cannot force stop package" + packageName, t);
        }
    }

    static boolean killNoFather() {
        if (killingFuture != null && killingFuture.getDelay(TimeUnit.SECONDS) > 0) {
            return false;
        }
        killingFuture = singleExecutor.schedule(new Runnable() {
            @Override
            public void run() {
                try {
                    dokillNoFather();
                } catch (Throwable t) { // NOSONAR
                    PreventLog.e("cannot killNoFather", t);
                }
            }
        }, TIME_KILL, TimeUnit.SECONDS);
        return true;
    }

    private static void dokillNoFather() {
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
            if (preventPackages.containsKey(name)) {
                return true;
            }
        }
        return false;
    }

}