package me.piebridge.forcestopgb.hook;

import android.app.ActivityManager;
import android.app.ActivityThread;
import android.app.Application;
import android.appwidget.AppWidgetManager;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageParser;
import android.database.Cursor;
import android.net.Uri;
import android.os.Binder;
import android.os.Handler;
import android.os.HandlerThread;
import android.os.Process;
import android.provider.Settings;
import android.text.TextUtils;
import android.util.Log;

import org.json.JSONObject;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.forcestopgb.common.CommonIntent;
import me.piebridge.forcestopgb.common.Packages;
import me.piebridge.forcestopgb.ui.Provider;
import me.piebridge.util.BroadcastFilterUtils;
import me.piebridge.util.HiddenAPI;
import me.piebridge.util.PackageUtils;
import me.piebridge.util.TaskRecordUtils;

import static me.piebridge.util.LogUtils.*;

public final class SystemHook {

    private static final String TAG = CommonIntent.TAG;

    private static boolean registered = false;
    private static boolean gotprevent = false;

    private static final int TIME_SUICIDE = 6;
    private static final int TIME_DESTROY = 6;
    private static final int TIME_PREVENT = 6;
    private static final int TIME_IMMEDIATE = 1;

    private static long lastChecking;
    private static long lastKilling;
    private static final int TIME_KILL = 1;
    private static final long MILLISECONDS = 1000;

    private static ActivityManager activityManager;

    private static Map<String, Boolean> preventPackages = new ConcurrentHashMap<String, Boolean>();

    private static Map<String, Integer> packageUids = new HashMap<String, Integer>();

    private static Map<String, Set<String>> abnormalProcesses = new ConcurrentHashMap<String, Set<String>>();

    private static Map<String, Map<Integer, AtomicInteger>> packageCounters = new ConcurrentHashMap<String, Map<Integer, AtomicInteger>>();

    private static ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(0x2);

    private static Set<String> SAFE_RECEIVER_ACTIONS = new HashSet<String>(Arrays.asList(
            // http://developer.android.com/guide/topics/appwidgets/index.html#Manifest
            // http://developer.android.com/reference/android/appwidget/AppWidgetManager.html#ACTION_APPWIDGET_UPDATE
            AppWidgetManager.ACTION_APPWIDGET_UPDATE
    ));

    private static ClassLoader classLoader;

    private static final int FIRST_APPLICATION_UID = 10000;

    private static Application application;
    private static BroadcastReceiver receiver;

    private SystemHook() {

    }

    public static void setClassLoader(ClassLoader classLoader) {
        SystemHook.classLoader = classLoader;
        for (String name : Packages.load()) {
            preventPackages.put(name, Boolean.TRUE);
        }
    }

    public static ClassLoader getClassLoader() {
        return SystemHook.classLoader;
    }

    private static class HookIntentFilter extends IntentFilter {
        public HookIntentFilter() {
            super();
            this.addAction(CommonIntent.ACTION_GET_PACKAGES);
            this.addAction(CommonIntent.ACTION_UPDATE_PREVENT);
            this.addAction(CommonIntent.ACTION_INCREASE_COUNTER);
            this.addAction(CommonIntent.ACTION_DECREASE_COUNTER);
            this.addAction(CommonIntent.ACTION_RESTART);
            this.addAction(CommonIntent.ACTION_ACTIVITY_DESTROY);
            this.addAction(CommonIntent.ACTION_FORCE_STOP);
            this.addDataScheme(CommonIntent.SCHEME);
        }
    }

    private static class HookBroadcastReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            String action = intent.getAction();
            String packageName = null;
            Uri data = intent.getData();
            if (data != null) {
                packageName = data.getSchemeSpecificPart();
            }
            if (CommonIntent.ACTION_GET_PACKAGES.equals(action)) {
                logRequest(action, packageName, -1);
                setResultData(new JSONObject(preventPackages).toString());
            } else if (CommonIntent.ACTION_UPDATE_PREVENT.equals(action)) {
                handleUpdatePrevent(action, packageName, intent);
            } else if (CommonIntent.ACTION_INCREASE_COUNTER.equals(action)) {
                handleIncreaseCounter(action, packageName, intent);
            } else if (CommonIntent.ACTION_DECREASE_COUNTER.equals(action)) {
                handleDecreaseCounter(action, packageName, intent);
            } else if (CommonIntent.ACTION_RESTART.equals(action)) {
                handleRestart(action, packageName);
            } else if (CommonIntent.ACTION_ACTIVITY_DESTROY.equals(action)) {
                handleDestroy(action, packageName);
            } else if (Intent.ACTION_PACKAGE_RESTARTED.equals(action)) {
                handlePackageRestarted(action, packageName);
            } else if (CommonIntent.ACTION_FORCE_STOP.equals(action)) {
                handleForceStop(action, packageName, intent);
            }
        }

        private void handleUpdatePrevent(String action, String packageName, Intent intent) {
            logRequest(action, packageName, -1);
            String[] packages = intent.getStringArrayExtra(CommonIntent.EXTRA_PACKAGES);
            boolean prevent = intent.getBooleanExtra(CommonIntent.EXTRA_PREVENT, true);
            for (String name : packages) {
                if (prevent) {
                    int count = countCounter(name);
                    preventPackages.put(name, count == 0);
                } else {
                    preventPackages.remove(name);
                }
            }
        }

        private void handleIncreaseCounter(String action, String packageName, Intent intent) {
            if (preventPackages.containsKey(packageName)) {
                preventPackages.put(packageName, Boolean.FALSE);
            }
            int uid = intent.getIntExtra(CommonIntent.EXTRA_UID, 0);
            int pid = intent.getIntExtra(CommonIntent.EXTRA_PID, 0);
            setPid(pid, packageName);
            if (uid > 0) {
                packageUids.put(packageName, uid);
            }
            Map<Integer, AtomicInteger> packageCounter = packageCounters.get(packageName);
            if (packageCounter == null) {
                packageCounter = new HashMap<Integer, AtomicInteger>();
                packageCounters.put(packageName, packageCounter);
            }
            AtomicInteger pidCounter = packageCounter.get(pid);
            if (pidCounter == null) {
                pidCounter = new AtomicInteger();
                packageCounter.put(pid, pidCounter);
            }
            pidCounter.incrementAndGet();
            int count = countCounter(packageName);
            logRequest(action, packageName, count);
        }

        private void handleDecreaseCounter(String action, String packageName, Intent intent) {
            Map<Integer, AtomicInteger> packageCounter = packageCounters.get(packageName);
            if (packageCounter != null) {
                int pid = intent.getIntExtra(CommonIntent.EXTRA_PID, 0);
                AtomicInteger pidCounter = packageCounter.get(pid);
                if (pidCounter != null) {
                    pidCounter.decrementAndGet();
                }
            }
            int count = countCounter(packageName);
            logRequest(action, packageName, count);
            if (count > 0) {
                return;
            }
            if (preventPackages.containsKey(packageName)) {
                preventPackages.put(packageName, Boolean.TRUE);
                logForceStop(action, packageName, "destroy if needed in " + TIME_DESTROY + "s");
                checkRunningServices(packageName, TIME_DESTROY);
            }
            killNoFather(packageName);
        }

        private void handleDestroy(String action, String packageName) {
            logRequest(action, packageName, -1);
            packageCounters.remove(packageName);
            if (preventPackages.containsKey(packageName)) {
                preventPackages.put(packageName, Boolean.TRUE);
                logForceStop(action, packageName, "destroy in " + TIME_SUICIDE + "s");
                forceStopPackageLater(packageName, TIME_SUICIDE);
            }
            killNoFather(packageName);
        }

        private void handleRestart(String action, String packageName) {
            if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
                preventPackages.put(packageName, Boolean.FALSE);
            }
            logRequest(action, packageName, -1);
        }

        private void handlePackageRestarted(String action, String packageName) {
            logRequest(action, packageName, -1);
            packageCounters.remove(packageName);
            if (preventPackages.containsKey(packageName)) {
                preventPackages.put(packageName, Boolean.TRUE);
            }
            killNoFather(packageName);
        }

        private void handleForceStop(String action, String packageName, Intent intent) {
            logRequest(action, packageName, -1);
            int uid = intent.getIntExtra(CommonIntent.EXTRA_UID, 0);
            packageCounters.remove(packageName);
            if (preventPackages.containsKey(packageName)) {
                preventPackages.put(packageName, Boolean.TRUE);
            }
            if (uid >= FIRST_APPLICATION_UID) {
                logForceStop(action, packageName, "force in " + TIME_IMMEDIATE + "s" + ", uid: " + uid);
                forceStopPackageForce(packageName, TIME_IMMEDIATE);
            }
        }
    }

    public static HookResult hookIntentFilter$match(Object filter, Object[] args) { // NOSONAR
        if (!isSystemHook()) {
            return HookResult.NONE;
        }

        String action = (String) args[0x0];

        if (BuildConfig.DEBUG) {
            logIntentFilter(action, filter, null);
        }

        if (filter instanceof PackageParser.ActivityIntentInfo) {
            // for receiver, we don't block for activity
            @SuppressWarnings("unchecked")
            PackageParser.Activity activity = ((PackageParser.ActivityIntentInfo) filter).activity;
            PackageParser.Package owner = activity.owner;
            String packageName = owner.applicationInfo.packageName;
            if (Boolean.TRUE.equals(preventPackages.get(packageName)) && owner.receivers.contains(activity)) {
                if (SAFE_RECEIVER_ACTIONS.contains(action)) {
                    return HookResult.NONE;
                }
                if (BuildConfig.DEBUG) {
                    logIntentFilter(true, filter, action, packageName);
                }
                return HookResult.NO_MATCH;
            }
        } else if (filter instanceof PackageParser.ServiceIntentInfo) {
            if (shouldIgnore(action)) {
                return HookResult.NO_MATCH;
            }
            // for service, we try to find calling package
            @SuppressWarnings("unchecked")
            PackageParser.Service service = ((PackageParser.ServiceIntentInfo) filter).service;
            PackageParser.Package owner = service.owner;
            ApplicationInfo ai = owner.applicationInfo;
            String packageName = ai.packageName;
            boolean prevents = Boolean.TRUE.equals(preventPackages.get(packageName));
            if (!prevents) {
                return HookResult.NONE;
            }
            if (Binder.getCallingUid() != Process.SYSTEM_UID) {
                if (BuildConfig.DEBUG) {
                    logIntentFilter(true, filter, action, packageName);
                }
                return HookResult.NO_MATCH;
            } else {
                logIntentFilter(false, filter, action, packageName);
            }
        } else if (Intent.ACTION_CLOSE_SYSTEM_DIALOGS.equals(action)) {
            // for dynamic broadcast, we only disable ACTION_CLOSE_SYSTEM_DIALOGS
            String packageName = BroadcastFilterUtils.getPackageName(filter);
            if (preventPackages.containsKey(packageName)) {
                logIntentFilter(true, filter, action, packageName);
                return HookResult.NO_MATCH;
            }
            if (BuildConfig.DEBUG) {
                logIntentFilter(action, filter, packageName);
            }
        }

        return HookResult.NONE;
    }

    private static boolean shouldIgnore(String action) {
        return shouldIgnoreLocation(action);
    }

    private static boolean shouldIgnoreLocation(String action) {
        if (application == null || action == null) {
            return false;
        }
        boolean enabled = Settings.Secure.getInt(application.getContentResolver(), Settings.Secure.LOCATION_MODE, Settings.Secure.LOCATION_MODE_OFF) != Settings.Secure.LOCATION_MODE_OFF;
        return !enabled && action.startsWith("com.android.location.service");
    }

    private static boolean registerReceiversIfNeeded() {
        if (registered) {
            return true;
        }

        HandlerThread thread = new HandlerThread("PreventService");
        thread.start();
        Handler handler = new Handler(thread.getLooper());

        receiver = new HookBroadcastReceiver();

        application = ActivityThread.currentApplication();
        application.registerReceiver(receiver, new HookIntentFilter(), null, handler);

        IntentFilter filter = new IntentFilter(Intent.ACTION_PACKAGE_RESTARTED);
        filter.addDataScheme("package");
        application.registerReceiver(receiver, filter, null, handler);

        registered = true;
        Log.i(TAG, "registered receiver");

        activityManager = (ActivityManager) application.getSystemService(Context.ACTIVITY_SERVICE);
        return false;
    }

    private static boolean retrievePreventsIfNeeded() {
        // this is for android 5.X, selinux deny the read file for app
        if (!preventPackages.isEmpty()) {
            return true;
        }
        if (application == null) {
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
                Cursor cursor = application.getContentResolver().query(Provider.CONTENT_URI, null, null, null, null);
                int index = cursor.getColumnIndex(Provider.COLUMN_PACKAGE);
                while (cursor.moveToNext()) {
                    String name = cursor.getString(index);
                    if (!preventPackages.containsKey(name)) {
                        preventPackages.put(name, Boolean.TRUE);
                    }
                }
                Log.i(TAG, "prevents: " + preventPackages.keySet().toString());
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

    public static boolean beforeActivityManagerService$startProcessLocked(Object[] args) { // NOSONAR
        if (!isSystemHook()) {
            return true;
        }

        registerReceiversIfNeeded();

        ApplicationInfo info = (ApplicationInfo) args[0x1];
        String hostingType = (String) args[0x4];
        ComponentName hostingName = (ComponentName) args[0x5];
        String packageName = info.packageName;

        if ("content provider".equals(hostingType)) {
            retrievePreventsIfNeeded();
        }

        if (BuildConfig.DEBUG) {
            Log.v(TAG, "startProcessLocked, type: " + hostingType + ", name: " + hostingName + ", info: " + info);
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

        // didn't handle for system package
        if (PackageUtils.isSystemPackage(application, packageName)) {
            return true;
        }

        // always block broadcast
        if ("broadcast".equals(hostingType)) {
            // for alarm
            forceStopPackageLaterIfPrevent(packageName, TIME_PREVENT);
            logStartProcess("disallow", packageName, hostingType, hostingName);
            return false;
        }

        // auto turn off service
        if ("service".equals(hostingType)) {
            checkRunningServices(packageName, TIME_PREVENT);
            logStartProcess("wont disallow", packageName, hostingType, hostingName);
        }

        return true;
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
                logForceStop("removeTask", packageName, "force in " + TIME_IMMEDIATE + "s");
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

    private static int countCounter(String packageName) {
        int count = 0;
        Map<Integer, AtomicInteger> values = packageCounters.get(packageName);
        if (values == null) {
            return count;
        }
        Iterator<Map.Entry<Integer, AtomicInteger>> iterator = values.entrySet().iterator();
        while (iterator.hasNext()) {
            Map.Entry<Integer, AtomicInteger> entry = iterator.next();
            if (checkPid(entry.getKey(), packageName)) {
                count += entry.getValue().get();
            } else {
                logIgnore(entry.getKey(), packageName);
                iterator.remove();
            }
        }
        return count;
    }

    private static String getPackage(int pid) {
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
            Log.e(TAG, "cannot read file " + file, e);
            return null;
        }
    }

    private static boolean isNormalProcessName(String processName, String packageName) {
        return (processName != null) && (processName.equals(packageName) || processName.startsWith(packageName + ":"));
    }

    private static boolean checkPid(int pid, String packageName) {
        Integer uid = packageUids.get(packageName);
        if (uid == null) {
            return false;
        }
        try {
            if (HiddenAPI.getUidForPid(pid) != uid) {
                return false;
            }
        } catch (Throwable t) { // NOSONAR
            Log.e(TAG, "cannot get uid for " + pid, t);
        }
        String processName = getPackage(pid);
        if (isNormalProcessName(processName, packageName)) {
            return true;
        }
        Set<String> abnormalPackages = abnormalProcesses.get(processName);
        return abnormalPackages != null && abnormalPackages.contains(packageName);
    }

    private static void setPid(int pid, String packageName) {
        String processName = getPackage(pid);
        if (processName != null && !isNormalProcessName(processName, packageName)) {
            Set<String> abnormalProcess = abnormalProcesses.get(processName);
            if (abnormalProcess == null) {
                abnormalProcess = new HashSet<String>();
                abnormalProcesses.put(processName, abnormalProcess);
            }
            if (abnormalProcess.add(packageName)) {
                Log.d(TAG, "package " + packageName + " has abnormal process: " + processName);
            }
        }
    }

    private static boolean checkRunningServices(final String packageName, int second) {
        if (activityManager == null) {
            Log.e(TAG, "activityManager is null, cannot check running services for " + packageName);
            return false;
        }
        long now = System.currentTimeMillis();
        if (now - lastChecking <= second * MILLISECONDS) {
            return false;
        }
        lastChecking = now;
        executor.schedule(new CheckingRunningService(packageName), second, TimeUnit.SECONDS);
        return true;
    }

    private static void forceStopPackageForce(final String packageName, int second) {
        executor.schedule(new Runnable() {
            @Override
            public void run() {
                if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
                    forceStopPackage(packageName);
                }
            }
        }, second, TimeUnit.SECONDS);
    }

    private static void forceStopPackageLater(final String packageName, int second) {
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

    private static void forceStopPackage(final String packageName) {
        if (!Boolean.TRUE.equals(preventPackages.get(packageName))) {
            return;
        }
        if (activityManager == null) {
            Log.e(TAG, "activityManager is null, cannot force stop package" + packageName);
            return;
        }
        try {
            HiddenAPI.forceStopPackage(activityManager, packageName);
            Log.i(TAG, "finish force stop package " + packageName);
            packageCounters.remove(packageName);
        } catch (Throwable t) { // NOSONAR
            Log.e(TAG, "cannot force stop package" + packageName, t);
        }
    }

    private static boolean killNoFather(final String packageName) {
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
                    Log.d(TAG, "cannot killNoFather", t);
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
                int uid = HiddenAPI.getUidForPid(pid);
                if (HiddenAPI.getParentPid(pid) == 1 && uid >= FIRST_APPLICATION_UID) {
                    Process.killProcess(pid);
                    logKill(pid, "without parent", getPackageName(uid, packageName));
                }
            }
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

    private static class CheckingRunningService implements Runnable {

        private final String packageName;

        private final Map<String, Boolean> serviceStatus;

        private CheckingRunningService(String packageName) {
            this.packageName = packageName;
            this.serviceStatus = new HashMap<String, Boolean>();
        }

        @Override
        public void run() {
            List<ActivityManager.RunningServiceInfo> services = activityManager.getRunningServices(Integer.MAX_VALUE);
            for (int i = services.size() - 1; i >= 0; --i) {
                ActivityManager.RunningServiceInfo service = services.get(i);
                String name = service.service.getPackageName();
                boolean prevents = Boolean.TRUE.equals(preventPackages.get(name));
                if (prevents || BuildConfig.DEBUG) {
                    Log.d(CommonIntent.TAG, "prevents: " + prevents + ", name: " + name + ", clientCount: " + service.clientCount + ", started: " + service.started + ", flags: " + service.flags + ", foreground: " + service.foreground);
                }
                if (prevents && (name.equals(this.packageName) || service.uid >= FIRST_APPLICATION_UID)) {
                    boolean canStop = service.started;
                    Boolean result = serviceStatus.get(name);
                    if (result == null || result) {
                        serviceStatus.put(name, canStop);
                    }
                }
            }
            stopServiceIfNeeded();
        }

        private void stopServiceIfNeeded() {
            for (Map.Entry<String, Boolean> entry : serviceStatus.entrySet()) {
                if (entry.getValue()) {
                    String name = entry.getKey();
                    Log.d(TAG, name + " has running services, force stop it");
                    forceStopPackage(name);
                }
            }
        }
    }

}