package me.piebridge.forcestopgb.hook;

import android.app.ActivityManager;
import android.app.ActivityThread;
import android.app.Application;
import android.appwidget.AppWidgetManager;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.PackageParser;
import android.os.Binder;
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
import me.piebridge.util.BroadcastFilterUtils;
import me.piebridge.util.HiddenAPI;
import me.piebridge.util.ProcessRecordUtils;

public final class SystemHook {

    private static final String TAG = CommonIntent.TAG;

    private static boolean registered = false;

    private static byte[] buffer = new byte[1024];

    private static ActivityManager activityManager;

    private static Map<String, Boolean> preventPackages;

    private static Map<String, Integer> packageUids = new HashMap<String, Integer>();

    private static Map<String, HashMap<Integer, AtomicInteger>> packageCounters = new ConcurrentHashMap<String, HashMap<Integer, AtomicInteger>>();

    private static ScheduledThreadPoolExecutor forceStopExecutor = new ScheduledThreadPoolExecutor(2);

    private static Set<String> SAVE_ACTIONS = new HashSet<String>(Arrays.asList(
            AppWidgetManager.ACTION_APPWIDGET_UPDATE
    ));

    private SystemHook() {

    }

    private static class HookIntentFilter extends IntentFilter {
        public HookIntentFilter() {
            super();
            this.addAction(CommonIntent.ACTION_GET_PACKAGES);
            this.addAction(CommonIntent.ACTION_UPDATE_PREVENT);
            this.addAction(CommonIntent.ACTION_INCREASE_COUNTER);
            this.addAction(CommonIntent.ACTION_DECREASE_COUNTER);
            this.addAction(Intent.ACTION_PACKAGE_RESTARTED);
            this.addAction(CommonIntent.ACTION_ACTIVITY_DESTROY);
            this.addAction(CommonIntent.ACTION_FORCE_STOP);
            this.addDataScheme("package");
        }
    }

    private static class HookBroadcaseReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            String action = intent.getAction();
            String packageName = intent.getData().getSchemeSpecificPart();
            if ("android".equals(packageName)) {
                return;
            }
            if (packageName != null && !packageCounters.containsKey(packageName)) {
                packageCounters.put(packageName, new HashMap<Integer, AtomicInteger>());
            }
            if (CommonIntent.ACTION_GET_PACKAGES.equals(action)) {
                logRequest(action, packageName, -1);
                setResultData(new JSONObject(preventPackages).toString());
            } else if (CommonIntent.ACTION_UPDATE_PREVENT.equals(action)) {
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
                forceStopExecutor.submit(new Runnable() {
                    @Override
                    public void run() {
                        Packages.save(preventPackages);
                    }
                });
            } else if (CommonIntent.ACTION_INCREASE_COUNTER.equals(action) || CommonIntent.ACTION_DECREASE_COUNTER.equals(action)) {
                int uid = intent.getIntExtra(CommonIntent.EXTRA_UID, 0);
                int pid = intent.getIntExtra(CommonIntent.EXTRA_PID, 0);
                if (uid > 0) {
                    packageUids.put(packageName, uid);
                }
                AtomicInteger pidCounter;
                HashMap<Integer, AtomicInteger> packageCounter = packageCounters.get(packageName);
                if (CommonIntent.ACTION_INCREASE_COUNTER.equals(action)) {
                    if (packageCounter == null) {
                        packageCounter = new HashMap<Integer, AtomicInteger>();
                        packageCounters.put(packageName, packageCounter);
                    }
                    pidCounter = packageCounter.get(pid);
                    if (pidCounter == null) {
                        pidCounter = new AtomicInteger();
                        packageCounter.put(pid, pidCounter);
                    }
                    pidCounter.incrementAndGet();
                } else if (packageCounter != null) {
                    pidCounter = packageCounter.get(pid);
                    if (pidCounter != null) {
                        pidCounter.decrementAndGet();
                    }
                }
                int count = countCounter(packageName);
                logRequest(action, packageName, count);
                if (!preventPackages.containsKey(packageName)) {
                    // do nothing
                } else if (count > 0) {
                    preventPackages.put(packageName, Boolean.FALSE);
                } else {
                    preventPackages.put(packageName, Boolean.TRUE);
                    logForceStop(action, packageName, "if needed");
                    forceStopPackageIfNeeded(packageName);
                }
            } else if (CommonIntent.ACTION_ACTIVITY_DESTROY.equals(action) || Intent.ACTION_PACKAGE_RESTARTED.equals(action)) {
                logRequest(action, packageName, -1);
                packageCounters.remove(packageName);
                if (preventPackages.containsKey(packageName)) {
                    preventPackages.put(packageName, Boolean.TRUE);
                    if (!Intent.ACTION_PACKAGE_RESTARTED.equals(action)) {
                        logForceStop(action, packageName, "if needed");
                        forceStopPackageIfNeeded(packageName);
                    }
                }
            } else if (CommonIntent.ACTION_FORCE_STOP.equals(action)) {
                logRequest(action, packageName, -1);
                packageCounters.remove(packageName);
                if (preventPackages.containsKey(packageName)) {
                    preventPackages.put(packageName, Boolean.TRUE);
                }
                logForceStop(action, packageName, "later");
                forceStopPackageLater(packageName);
            }
        }
    }

    private static void loadPreventPackagesIfNeeded() {
        if (preventPackages == null && isSystemHook()) {
            Log.d(TAG, "load prevent packages");
            preventPackages = Packages.load();
        }
    }

    private static void registerHookReceiverIfNeeded() {
        if (isSystemHook() && !registered) {
            Application application = ActivityThread.currentApplication();
            application.registerReceiver(new HookBroadcaseReceiver(), new HookIntentFilter());
            registered = true;
            Log.d(TAG, "registered receiver");
        }
    }

    public static HookResult hookIntentFilter$match(Object filter, Object... args) {
        String action = (String) args[0];

        if (CommonIntent.ACTION_CHECK_HOOK.equals(action)) {
            return HookResult.HOOK_ENABLED;
        }

        if (!isSystemHook()) {
            return HookResult.None;
        }

        if (action.startsWith(CommonIntent.ACTION_NAMESPACE)) {
            return HookResult.None;
        }

        if (SAVE_ACTIONS.contains(action)) {
            return HookResult.None;
        }

        if (filter instanceof PackageParser.ActivityIntentInfo) {
            // for receiver
            @SuppressWarnings("unchecked")
            PackageParser.Activity activity = ((PackageParser.ActivityIntentInfo) filter).activity;
            PackageParser.Package owner = activity.owner;
            String packageName = owner.applicationInfo.packageName;
            loadPreventPackagesIfNeeded();
            if (Boolean.TRUE.equals(preventPackages.get(packageName)) && owner.receivers.contains(activity)) {
                if (BuildConfig.DEBUG) {
                    logDisallow(filter.toString(), action, packageName);
                }
                return HookResult.NO_MATCH;
            }
        } else if (filter instanceof PackageParser.ServiceIntentInfo && Binder.getCallingUid() != Process.SYSTEM_UID) {
            // for service
            PackageParser.Service service = ((PackageParser.ServiceIntentInfo) filter).service;
            PackageParser.Package owner = service.owner;
            String packageName = owner.applicationInfo.packageName;
            loadPreventPackagesIfNeeded();
            if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
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
                Log.v(TAG, "filter:  " + filter + ", action: " + action + ", package: " + packageName);
            }
        }

        return HookResult.None;
    }

    public static boolean beforeActivityManagerService$startProcessLocked(Object[] args) {
        if (!isSystemHook()) {
            Log.e(SystemHook.TAG, "non-system call for ActivityManagerService$startProcessLocked");
            return true;
        }
        registerHookReceiverIfNeeded();
        Object app = args[0];
        String hostingType = (String) args[1];
        String hostingName = (String) args[2];
        String packageName = ProcessRecordUtils.getPackageName(app);
        if (packageName == null) {
            Log.w(SystemHook.TAG, "cannot get package name from " + app);
            return true;
        }
        boolean disallow = "broadcast".equals(hostingType);
        loadPreventPackagesIfNeeded();
        if (disallow && Boolean.TRUE.equals(preventPackages.get(packageName))) {
            ProcessRecordUtils.setPid(app, 0);
            forceStopPackageLaterIfPrevent(packageName);
            logStartProcess("disallow", packageName, hostingType, hostingName);
            return false;
        } else {
            if ("activity".equals(hostingType) && Boolean.TRUE.equals(preventPackages.get(packageName))) {
                preventPackages.put(packageName, Boolean.FALSE);
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
        HashMap<Integer, AtomicInteger> values = packageCounters.get(packageName);
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

    private static boolean isZombie(int pid) {
        File file = new File(new File("/proc", String.valueOf(pid)), "stat");
        String content = getContent(file);
        return content != null && content.contains(" Z ");
    }

    private static String getPackage(int pid) {
        File file = new File(new File("/proc", String.valueOf(pid)), "cmdline");
        return getContent(file);
    }

    private synchronized static String getContent(File file) {
        if (!file.isFile() || !file.canRead()) {
            return null;
        }

        try {
            InputStream is = new BufferedInputStream(new FileInputStream(file));
            ByteArrayOutputStream os = new ByteArrayOutputStream();
            try {
                int length;
                while ((length = is.read(buffer)) != -1) {
                    os.write(buffer, 0, length);
                }
            } finally {
                is.close();
            }
            return os.toString().trim();
        } catch (IOException e) {
            return null;
        }
    }

    private static boolean checkPid(int pid, String packageName) {
        String processName = getPackage(pid);
        Integer uid = packageUids.get(packageName);
        return processName != null && uid != null && processName.startsWith(packageName) && HiddenAPI.getUidForPid(pid) == uid;
    }

    private static void forceStopPackageIfNeeded(final String packageName) {
        forceStopExecutor.schedule(new Runnable() {
            @Override
            public void run() {
                checkAndForceStopPackage(packageName);
            }
        }, 400, TimeUnit.MILLISECONDS);
    }

    private static void forceStopPackageLater(final String packageName) {
        forceStopExecutor.schedule(new Runnable() {
            @Override
            public void run() {
                if (Boolean.FALSE.equals(preventPackages.get(packageName))) {
                    return;
                }
                int count = countCounter(packageName);
                if (count != 0) {
                    packageCounters.remove(packageName);
                    forceStopPackage(packageName);
                } else {
                    checkAndForceStopPackage(packageName);
                }
            }
        }, 400, TimeUnit.MILLISECONDS);
    }

    private static void forceStopPackageLaterIfPrevent(final String packageName) {
        forceStopExecutor.schedule(new Runnable() {
            @Override
            public void run() {
                if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
                    forceStopPackage(packageName);
                }
            }
        }, 400, TimeUnit.MILLISECONDS);
    }

    private static void checkAndForceStopPackage(String packageName) {
        if (Boolean.FALSE.equals(preventPackages.get(packageName))) {
            return;
        }
        for (ActivityManager.RunningServiceInfo service : getActivityManager().getRunningServices(Integer.MAX_VALUE)) {
            if (service.service.getPackageName().equals(packageName)) {
                Log.d(TAG, packageName + " has running services, force stop it");
                forceStopPackage(packageName);
                return;
            }
        }
        Log.d(TAG, packageName + " has no running services");
        killNoFather(packageName);
    }

    private static ActivityManager getActivityManager() {
        if (activityManager == null) {
            activityManager = (ActivityManager) ActivityThread.currentApplication().getSystemService(Context.ACTIVITY_SERVICE);
        }
        return activityManager;
    }

    private static void forceStopPackage(final String packageName) {
        if (Boolean.FALSE.equals(preventPackages.get(packageName))) {
            return;
        }
        try {
            HiddenAPI.forceStopPackage(getActivityManager(), packageName);
            Log.i(TAG, "finish force stop package " + packageName);
        } catch (Throwable t) {
            Log.e(TAG, "cannot force stop package" + packageName, t);
        }
        killNoFather(packageName);
    }

    public static void resetPackages() {
        Log.d(TAG, "resetPackages");
        Packages.resetPackages();
    }

    private static boolean killNoFather(String packageName) {
        Integer uid = packageUids.get(packageName);
        if (uid == null) {
            return false;
        } else {
            try {
                killNoFather(uid, packageName);
            } catch (Throwable t) {
                Log.d(TAG, "cannot killNoFather for " + uid, t);
            }
            return true;
        }
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
//                } else if (isZombie(pid)) {
//                    Process.killProcess(pid);
//                    logKill(pid, "zombie", packageName);
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
        sb.append("), package: ");
        sb.append(packageName);
        Log.d(TAG, sb.toString());
    }

    private static void logForceStop(String action, String packageName, String message) {
        StringBuilder sb = new StringBuilder();
        sb.append("action: ");
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
        sb.append("action: ");
        sb.append(action);
        sb.append(", packageName: ");
        sb.append(packageName);
        if (count >= 0) {
            sb.append(", count: ");
            sb.append(count);
        }
        Log.d(TAG, sb.toString());
    }

    private static void logUnknown(final String filter, final String action) {
        StringBuilder sb = new StringBuilder();
        sb.append("cannot get package from ");
        sb.append(filter);
        sb.append(", action: ");
        sb.append(action);
        Log.w(TAG, sb.toString());
    }

    private static void logDisallow(final String filter, final String action, final String packageName) {
        StringBuilder sb = new StringBuilder();
        sb.append("disallow ");
        sb.append(filter);
        sb.append(", action: ");
        sb.append(action);
        sb.append(", package: ");
        sb.append(packageName);
        sb.append(", callingUid: ");
        sb.append(Binder.getCallingUid());
        sb.append(", callingPid: ");
        sb.append(Binder.getCallingPid());
        Log.v(TAG, sb.toString());
    }

    private static void logStartProcess(final String allow, final String packageName, final String hostingType, final String hostingName) {
        StringBuilder sb = new StringBuilder();
        sb.append(allow);
        sb.append(" start ");
        sb.append(packageName);
        sb.append(" for ");
        sb.append(hostingType);
        sb.append(" ");
        sb.append(hostingName);
        Log.d(TAG, sb.toString());
    }

}