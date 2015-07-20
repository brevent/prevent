package me.piebridge.forcestopgb.hook;

import android.app.ActivityManager;
import android.app.ActivityThread;
import android.app.Application;
import android.appwidget.AppWidgetManager;
import android.content.BroadcastReceiver;
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
import android.text.TextUtils;
import android.util.Log;

import org.json.JSONObject;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
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
import me.piebridge.util.ProcessRecordUtils;

public final class SystemHook {

    private static final String TAG = CommonIntent.TAG;

    private static boolean registered = false;
    private static boolean gotprevent = false;
    private static boolean firststart = true;

    private static final int TIME_SUICIDE = 6;
    private static final int TIME_DESTROY = 6;
    private static final int TIME_PREVENT = 60;
    private static final int TIME_IMMEDIATE = 1;

    private static final String ACTION = "action: ";
    private static final String FILTER = "filter: ";
    private static final String PACKAGE = "package: ";

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

    private static final int FLAG_SYSTEM_APP = ApplicationInfo.FLAG_SYSTEM | ApplicationInfo.FLAG_UPDATED_SYSTEM_APP;

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
                handleForceStop(action, packageName);
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
                forceStopPackageIfNeeded(packageName, TIME_DESTROY);
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
        }

        private void handleForceStop(String action, String packageName) {
            logRequest(action, packageName, -1);
            packageCounters.remove(packageName);
            if (preventPackages.containsKey(packageName)) {
                preventPackages.put(packageName, Boolean.TRUE);
            }
            logForceStop(action, packageName, "force in " + TIME_IMMEDIATE + "s");
            forceStopPackageForce(packageName, TIME_IMMEDIATE);
            killNoFather(packageName);
        }
    }

    public static HookResult hookIntentFilter$match(Object filter, Object[] args) { // NOSONAR
        if (!isSystemHook()) {
            return HookResult.NONE;
        }

        String action = (String) args[0x0];

        if (BuildConfig.DEBUG) {
            StringBuilder sb = new StringBuilder();
            sb.append(ACTION);
            sb.append(action);
            sb.append(", ");
            sb.append(FILTER);
            sb.append(filter);
            sb.append(", callingUid: ");
            sb.append(Binder.getCallingUid());
            sb.append(", callingPid: ");
            sb.append(Binder.getCallingPid());
            Log.v(TAG, sb.toString());
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
                    logDisallow(filter.toString(), action, packageName);
                }
                return HookResult.NO_MATCH;
            }
        } else if (filter instanceof PackageParser.ServiceIntentInfo) {
            // for service, we try to find calling package
            @SuppressWarnings("unchecked")
            PackageParser.Service service = ((PackageParser.ServiceIntentInfo) filter).service;
            PackageParser.Package owner = service.owner;
            String packageName = owner.applicationInfo.packageName;
            if (!isSystemPackage(owner) && Binder.getCallingUid() != Process.SYSTEM_UID && Boolean.TRUE.equals(preventPackages.get(packageName))) {
                if (BuildConfig.DEBUG) {
                    logDisallow(filter.toString(), action, packageName);
                }
                return HookResult.NO_MATCH;
            }
        } else if (Intent.ACTION_CLOSE_SYSTEM_DIALOGS.equals(action)) {
            // for dynamic broadcast, we only disable ACTION_CLOSE_SYSTEM_DIALOGS
            String packageName = BroadcastFilterUtils.getPackageName(filter);
            if (preventPackages.containsKey(packageName)) {
                logDisallow(filter.toString(), action, packageName);
                return HookResult.NO_MATCH;
            }
            if (BuildConfig.DEBUG) {
                Log.d(TAG, ACTION + action + ", filter:  " + filter + ", package: " + packageName);
            }
        }

        return HookResult.NONE;
    }

    private static boolean isSystemPackage(PackageParser.Package owner) {
        return (owner.applicationInfo.flags & FLAG_SYSTEM_APP) != 0;
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
        Log.d(TAG, "registered receiver");

        activityManager = (ActivityManager) application.getSystemService(Context.ACTIVITY_SERVICE);
        return false;
    }

    private static boolean retrievePreventsIfNeeded() {
        // i think, the file can be read
        // this is for android 5.X, selinux deny the read file for app
        if (!preventPackages.isEmpty()) {
            return true;
        }
        if (firststart) {
            firststart = false;
            return false;
        }
        if (gotprevent) {
            return true;
        }
        if (application == null) {
            return false;
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
                Log.d(TAG, "prevents: " + preventPackages.keySet().toString());
            }
        });
    }

    public static boolean beforeActivityManagerService$startProcessLocked(Object[] args) { // NOSONAR
        if (!isSystemHook()) {
            return true;
        }

        registerReceiversIfNeeded();
        retrievePreventsIfNeeded();

        Object app = args[0x0];
        String hostingType = (String) args[0x1];
        String hostingName = (String) args[0x2];
        String packageName = ProcessRecordUtils.getPackageName(app);

        if (BuildConfig.DEBUG) {
            Log.v(TAG, "startProcessLocked, type: " + hostingType + ", name: " + hostingName + ", app: " + app);
        }
        boolean disallow = "broadcast".equals(hostingType);
        if (disallow && Boolean.TRUE.equals(preventPackages.get(packageName))) {
            ProcessRecordUtils.setPid(app, 0);
            forceStopPackageLaterIfPrevent(packageName, TIME_PREVENT);
            logStartProcess("disallow", packageName, hostingType, hostingName);
            return false;
        } else {
            if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
                if ("activity".equals(hostingType)) {
                    preventPackages.put(packageName, Boolean.FALSE);
                } else if ("service".equals(hostingType)) {
                    logStartProcess("wont disallow", packageName, hostingType, hostingName);
                }
            }
            if (BuildConfig.DEBUG) {
                logStartProcess("allow", packageName, hostingType, hostingName);
            }
            return true;
        }
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

    private static void forceStopPackageIfNeeded(final String packageName, int second) {
        if (activityManager == null) {
            Log.e(TAG, "activityManager is null, cannot check running services for " + packageName);
            return;
        }
        executor.schedule(new Runnable() {
            @Override
            public void run() {
                if (!Boolean.TRUE.equals(preventPackages.get(packageName))) {
                    return;
                }
                for (ActivityManager.RunningServiceInfo service : activityManager.getRunningServices(Integer.MAX_VALUE)) {
                    if (service.service.getPackageName().equals(packageName)) {
                        Log.d(TAG, packageName + " has running services, force stop it");
                        forceStopPackage(packageName);
                        return;
                    }
                }
                Log.d(TAG, packageName + " has no running services");

            }
        }, second, TimeUnit.SECONDS);
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
        final Integer uid = packageUids.get(packageName);
        if (uid == null || uid < FIRST_APPLICATION_UID) {
            return false;
        }
        executor.submit(new Runnable() {
            @Override
            public void run() {
                try {
                    killNoFather(uid, packageName);
                } catch (Throwable t) { // NOSONAR
                    Log.d(TAG, "cannot killNoFather for " + uid, t);
                }
            }
        });
        return true;
    }

    private static void killNoFather(int uid, String packageName) {
        File proc = new File("/proc");
        for (File file : proc.listFiles()) {
            if (file.isDirectory() && TextUtils.isDigitsOnly(file.getName())) {
                int pid = Integer.parseInt(file.getName());
                if (HiddenAPI.getUidForPid(pid) != uid) {
                    continue;
                }
                if (HiddenAPI.getParentPid(pid) == 1) {
                    Process.killProcess(pid);
                    logKill(pid, "without parent", packageName);
                }
            }
        }
    }

    private static void logKill(int pid, String reason, String packageName) {
        StringBuilder sb = new StringBuilder();
        sb.append("kill ");
        sb.append(pid);
        sb.append("(");
        sb.append(reason);
        sb.append("), ");
        sb.append(PACKAGE);
        sb.append(packageName);
        Log.d(TAG, sb.toString());
    }

    private static void logForceStop(String action, String packageName, String message) {
        StringBuilder sb = new StringBuilder();
        sb.append(ACTION);
        sb.append(action);
        sb.append(", force stop ");
        sb.append(packageName);
        sb.append(" ");
        sb.append(message);
        Log.d(TAG, sb.toString());
    }

    private static void logIgnore(int key, String packageName) {
        StringBuilder sb = new StringBuilder();
        sb.append("pid ");
        sb.append(key);
        sb.append(" is not for ");
        sb.append(packageName);
        Log.d(TAG, sb.toString());
    }

    private static void logRequest(String action, String packageName, int count) {
        StringBuilder sb = new StringBuilder();
        sb.append(ACTION);
        sb.append(action);
        sb.append(", ");
        sb.append(PACKAGE);
        sb.append(packageName);
        if (count >= 0) {
            sb.append(", count: ");
            sb.append(count);
        }
        Log.i(TAG, sb.toString());
    }

    private static void logDisallow(final String filter, final String action, final String packageName) {
        StringBuilder sb = new StringBuilder();
        sb.append("disallow ");
        sb.append(ACTION);
        sb.append(action);
        sb.append(", ");
        sb.append(FILTER);
        sb.append(filter);
        sb.append(", ");
        sb.append(PACKAGE);
        sb.append(packageName);
        if (BuildConfig.DEBUG) {
            sb.append(", callingUid: ");
            sb.append(Binder.getCallingUid());
            sb.append(", callingPid: ");
            sb.append(Binder.getCallingPid());
        }
        Log.d(TAG, sb.toString());
    }

    private static void logStartProcess(final String allow, final String packageName, final String hostingType, final String hostingName) {
        StringBuilder sb = new StringBuilder();
        sb.append(allow);
        sb.append(" start ");
        sb.append(packageName);
        sb.append(" for");
        if (hostingType != null) {
            sb.append(" ");
            sb.append(hostingType);
        }
        if (hostingName != null) {
            sb.append(" ");
            sb.append(hostingName);
        }
        Log.d(TAG, sb.toString());
    }

}