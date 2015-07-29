package me.piebridge.prevent.framework;

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
import android.content.pm.ResolveInfo;
import android.database.Cursor;
import android.os.Binder;
import android.os.Handler;
import android.os.HandlerThread;
import android.os.Process;
import android.provider.Settings;
import android.text.TextUtils;
import android.util.SparseArray;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.common.GmsUtils;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.framework.util.BroadcastFilterUtils;
import me.piebridge.prevent.framework.util.HideApiUtils;
import me.piebridge.prevent.framework.util.LogUtils;
import me.piebridge.prevent.framework.util.TaskRecordUtils;
import me.piebridge.prevent.ui.PreventProvider;
import me.piebridge.prevent.common.PackageUtils;

public final class SystemHook {

    private static boolean registered = false;
    private static boolean gotprevent = false;

    static final int TIME_SUICIDE = 6;
    static final int TIME_DESTROY = 6;
    static final int TIME_PREVENT = 12;
    static final int TIME_IMMEDIATE = 1;
    static final int TIME_CHECK_SERVICE = 30;

    private static long lastChecking;
    private static long lastKilling;
    static final int TIME_KILL = 1;
    static final long MILLISECONDS = 1000;

    static final int FIRST_APPLICATION_UID = 10000;

    private static Context mContext;

    static Map<String, Boolean> preventPackages = new ConcurrentHashMap<String, Boolean>();

    static Map<String, Integer> packageUids = new HashMap<String, Integer>();

    static SparseArray<Boolean> gmsUids = new SparseArray<Boolean>();

    static Map<String, Map<Integer, AtomicInteger>> packageCounters = new ConcurrentHashMap<String, Map<Integer, AtomicInteger>>();

    static Set<String> checkingPackageNames = new TreeSet<String>();
    static final Object CHECKING_LOCK = new Object();

    private static ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(0x2);

    private static Map<ComponentName, Boolean> widgets = new HashMap<ComponentName, Boolean>();

    private static ClassLoader classLoader;

    private static Handler mHandler;

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
                // http://developer.android.com/guide/topics/appwidgets/index.html#Manifest
                // http://developer.android.com/reference/android/appwidget/AppWidgetManager.html#ACTION_APPWIDGET_UPDATE
                if (AppWidgetManager.ACTION_APPWIDGET_UPDATE.contains(action)) {
                    return IntentFilterMatchResult.NONE;
                }
                if (BuildConfig.DEBUG) {
                    LogUtils.logIntentFilter(true, filter, action, packageName);
                }
                return IntentFilterMatchResult.NO_MATCH;
            }
        } else if (filter instanceof PackageParser.ServiceIntentInfo) {
            // for service, we try to find calling package
            if (shouldIgnoreLocation(action)) {
                return IntentFilterMatchResult.NO_MATCH;
            }
            @SuppressWarnings("unchecked")
            PackageParser.Service service = ((PackageParser.ServiceIntentInfo) filter).service;
            PackageParser.Package owner = service.owner;
            ApplicationInfo ai = owner.applicationInfo;
            if (canUseGms(ai)) {
                return IntentFilterMatchResult.NONE;
            }
            String packageName = ai.packageName;
            boolean prevents = Boolean.TRUE.equals(preventPackages.get(packageName));
            if (!prevents) {
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

    public static boolean registerReceiversIfNeeded(Context context) {
        if (registered) {
            return true;
        }

        if (context == null) {
            mContext = ActivityThread.currentApplication();
        } else {
            mContext = context;
        }

        if (mHandler == null) {
            HandlerThread thread = new HandlerThread("PreventService");
            thread.start();
            mHandler = new Handler(thread.getLooper());
        }

        BroadcastReceiver receiver = new SystemReceiver(mContext);

        IntentFilter manager = new IntentFilter();
        manager.addAction(PreventIntent.ACTION_GET_PACKAGES);
        manager.addAction(PreventIntent.ACTION_GET_PROCESSES);
        manager.addAction(PreventIntent.ACTION_UPDATE_PREVENT);
        manager.addDataScheme(PreventIntent.SCHEME);

        try {
            mContext.registerReceiver(receiver, manager, PreventIntent.PERMISSION_MANAGER, mHandler);
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
        mContext.registerReceiver(receiver, hook, null, mHandler);

        IntentFilter filter = new IntentFilter(Intent.ACTION_PACKAGE_RESTARTED);
        filter.addDataScheme("package");
        mContext.registerReceiver(receiver, filter, null, mHandler);

        registered = true;
        PreventLog.i("registered receiver");

        return false;
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
        executor.submit(new Runnable() {
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

    private static boolean canUseGms(int uid) {
        int callingUid = Binder.getCallingUid();
        Boolean value = gmsUids.get(callingUid);
        if (value != null) {
            return value;
        }
        PackageManager pm = mContext.getPackageManager();
        String[] packageNames = pm.getPackagesForUid(callingUid);
        if (packageNames == null || packageNames.length == 0) {
            return false;
        }
        if (pm.checkSignatures(callingUid, uid) == PackageManager.SIGNATURE_MATCH) {
            PreventLog.d("allow " + packageNames[0] + "(same signature) with gms to use gms if needed");
            gmsUids.put(callingUid, true);
        } else {
            gmsUids.put(callingUid, packageNames.length == 1 && canUseGms(pm, packageNames[0]));
        }
        return gmsUids.get(callingUid);
    }

    private static boolean canUseGms(PackageManager pm, String packageName) {
        if (pm.getLaunchIntentForPackage(packageName) == null || !packageName.startsWith(GmsUtils.GAPPS_PREFIX)) {
            return false;
        } else {
            PreventLog.d("allow " + packageName + " to use gms if needed");
            return true;
        }
    }

    private static boolean isSystemApps(PackageManager pm, String packageName) {
        try {
            if (PackageUtils.isSystemPackage(pm.getApplicationInfo(packageName, 0).flags)) {
                PreventLog.d("allow system package " + packageName + " to use gms if needed");
                return true;
            }
        } catch (PackageManager.NameNotFoundException e) { // NOSONAR
            PreventLog.d("cannot find package: " + packageName);
        }
        return false;
    }

    private static boolean canUseGms(ApplicationInfo info) {
        // only for system package, or only for gms?
        return mContext != null && GmsUtils.GMS.equals(info.packageName) && canUseGms(info.uid);
    }

    public static boolean beforeActivityManagerService$startProcessLocked(Object[] args) { // NOSONAR
        if (!isSystemHook()) {
            return true;
        }

        registerReceiversIfNeeded(null);

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
            checkRunningServices(GmsUtils.GMS.equals(packageName) ? null : packageName);
            if (isWidget(hostingName)) {
                LogUtils.logStartProcess(false, packageName, hostingType + "(widget)", hostingName);
                return true;
            } else {
                forceStopPackageLaterIfPrevent(packageName, TIME_PREVENT);
                LogUtils.logStartProcess(true, packageName, hostingType, hostingName);
                return false;
            }
        }

        // auto turn off service
        if ("service".equals(hostingType)) {
            checkRunningServices(GmsUtils.GMS.equals(packageName) ? null : packageName);
            LogUtils.logStartProcess(false, packageName, hostingType, hostingName);
        }

        return true;
    }

    private static boolean isWidget(ComponentName cn) {
        Boolean result = widgets.get(cn);
        if (result != null) {
            return result;
        }
        result = false;
        Intent intent = new Intent();
        intent.setComponent(cn);
        for (ResolveInfo info : mContext.getPackageManager().queryBroadcastReceivers(intent, 0)) {
            if (info != null && info.filter != null && info.filter.matchAction(AppWidgetManager.ACTION_APPWIDGET_UPDATE)) {
                PreventLog.d("info " + info + " match action: " + AppWidgetManager.ACTION_APPWIDGET_UPDATE);
                result = true;
                break;
            }
        }
        widgets.put(cn, result);
        return result;
    }

    public static void afterActivityManagerService$cleanUpRemovedTaskLocked(Object[] args) { // NOSONAR
        String packageName = TaskRecordUtils.getPackageName(args[0]);
        if (packageName != null) {
            autoPrevents(packageName);
        }
    }

    private static void autoPrevents(final String packageName) {
        executor.submit(new Runnable() {
            @Override
            public void run() {
                packageCounters.remove(packageName);
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

    static boolean checkRunningServices(final String packageName) {
        if (mContext == null) {
            PreventLog.e("context is null, cannot check running services for " + packageName);
            return false;
        }
        long now = System.currentTimeMillis();
        if (now - lastChecking <= TIME_CHECK_SERVICE * MILLISECONDS) {
            return false;
        }
        lastChecking = now;
        if (packageName != null) {
            synchronized (CHECKING_LOCK) {
                checkingPackageNames.add(packageName);
            }
        }
        executor.schedule(new CheckingRunningService(mContext, checkingPackageNames), TIME_CHECK_SERVICE, TimeUnit.SECONDS);
        return true;
    }

    static void forceStopPackageForce(final String packageName, int second) {
        executor.schedule(new Runnable() {
            @Override
            public void run() {
                if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
                    forceStopPackage(packageName);
                }
            }
        }, second, TimeUnit.SECONDS);
    }

    static void forceStopPackageLater(final String packageName, int second) {
        executor.schedule(new Runnable() {
            @Override
            public void run() {
                if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
                    forceStopPackage(packageName);
                }
            }
        }, second, TimeUnit.SECONDS);
    }

    private static void forceStopPackageLaterIfPrevent(final String packageName, int second) {
        executor.schedule(new Runnable() {
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
            ActivityManager activityManager = (ActivityManager) mContext.getSystemService(Context.ACTIVITY_SERVICE);
            HideApiUtils.forceStopPackage(activityManager, packageName);
            packageCounters.remove(packageName);
        } catch (Throwable t) { // NOSONAR
            PreventLog.e("cannot force stop package" + packageName, t);
        }
    }

    static boolean killNoFather(final String packageName) {
        long now = System.currentTimeMillis();
        if (now - lastKilling <= TIME_KILL * MILLISECONDS) {
            return false;
        }
        lastKilling = now;
        executor.schedule(new Runnable() {
            @Override
            public void run() {
                try {
                    dokillNoFather(packageName);
                } catch (Throwable t) { // NOSONAR
                    PreventLog.e("cannot killNoFather", t);
                }
            }
        }, TIME_KILL, TimeUnit.SECONDS);
        return true;
    }

    private static void dokillNoFather(String packageName) {
        File proc = new File("/proc");
        for (File file : proc.listFiles()) {
            if (file.isDirectory() && TextUtils.isDigitsOnly(file.getName())) {
                int pid = Integer.parseInt(file.getName());
                int uid = HideApiUtils.getUidForPid(pid);
                if (HideApiUtils.getParentPid(pid) == 1 && uid >= FIRST_APPLICATION_UID) {
                    killIfNeed(uid, pid, packageName);
                }
            }
        }
    }

    private static void killIfNeed(int uid, int pid, String packageName) {
        String name = getPackageName(uid, packageName);
        if (name == null || preventPackages.containsKey(name)) {
            Process.killProcess(pid);
            if (name == null) {
                name = "(uid: " + uid + ", process: + " + getProcessName(pid) + ")";
            }
            LogUtils.logKill(pid, "without parent", name);
        }
    }

    private static String getPackageName(int uid, String packageName) {
        Integer currentUid = packageUids.get(packageName);
        if (currentUid != null && currentUid == uid) {
            return packageName;
        }
        for (Map.Entry<String, Integer> entry : packageUids.entrySet()) {
            if (entry.getValue().equals(uid)) {
                return entry.getKey();
            }
        }
        return null;
    }

    private static boolean shouldIgnoreLocation(String action) {
        if (mContext == null || action == null) {
            return false;
        }
        boolean disabled = Settings.Secure.getInt(mContext.getContentResolver(), Settings.Secure.LOCATION_MODE, Settings.Secure.LOCATION_MODE_OFF) == Settings.Secure.LOCATION_MODE_OFF;
        return disabled && (action.startsWith("com.android.location.service") || action.startsWith("com.google.android.location"));
    }

}