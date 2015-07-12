package me.piebridge.forcestopgb.hook;


import org.json.JSONObject;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import android.app.ActivityManager;
import android.app.ActivityThread;
import android.app.Application;
import android.appwidget.AppWidgetManager;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.os.Process;
import android.text.TextUtils;
import android.util.Log;
import android.view.inputmethod.InputMethod;

import me.piebridge.forcestopgb.common.CommonIntent;
import me.piebridge.util.HiddenAPI;
import me.piebridge.util.ReflectUtil;

public final class SystemHook {

    private static final String TAG = CommonIntent.TAG;

    private static boolean registered = false;

    private static byte[] buffer = new byte[1024];

    private static ActivityManager activityManager;

    private static Map<String, Boolean> preventPackages;

    private static Map<String, Integer> packageUids = new HashMap<String, Integer>();

    private static Map<String, HashMap<Integer, AtomicInteger>> packageCounters = new ConcurrentHashMap<String, HashMap<Integer, AtomicInteger>>();

    private static ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(2);

    private SystemHook() {

    }

    private static class HookIntentFilter extends IntentFilter {
        public HookIntentFilter() {
            super();
            this.addAction(CommonIntent.ACTION_GET_PACKAGES);
            this.addAction(CommonIntent.ACTION_UPDATE_PREVENT);
            this.addAction(CommonIntent.ACTION_UPDATE_COUNTER);
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
            if (packageName != null && !packageCounters.containsKey(packageName)) {
                packageCounters.put(packageName, new HashMap<Integer, AtomicInteger>());
            }
            if (CommonIntent.ACTION_GET_PACKAGES.equals(action)) {
                setResultData(new JSONObject(preventPackages).toString());
            } else if (CommonIntent.ACTION_UPDATE_PREVENT.equals(action)) {
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
                executor.submit(new Runnable() {
                    @Override
                    public void run() {
                        Packages.save(preventPackages);
                    }
                });
            } else if (CommonIntent.ACTION_UPDATE_COUNTER.equals(action)) {
                int pid = intent.getIntExtra(CommonIntent.EXTRA_PID, 0);
                int delta = intent.getIntExtra(CommonIntent.EXTRA_DELTA, 0);
                if (pid > 0) {
                    if (!packageCounters.get(packageName).containsKey(pid)) {
                        packageCounters.get(packageName).put(pid, new AtomicInteger());
                    }
                    packageCounters.get(packageName).get(pid).addAndGet(delta);
                }
                int count = countCounter(packageName);
                Log.d(TAG, "action: " + action + ", package: " + packageName + ", count: " + count);
                if (!preventPackages.containsKey(packageName)) {
                    // do nothing
                } else if (count > 0) {
                    preventPackages.put(packageName, Boolean.FALSE);
                } else {
                    preventPackages.put(packageName, Boolean.TRUE);
                    Log.d(TAG, "count is 0, force stop " + packageName + " if needed");
                    forceStopPackageIfNeeded(packageName);
                }
            } else if (CommonIntent.ACTION_ACTIVITY_DESTROY.equals(action) || Intent.ACTION_PACKAGE_RESTARTED.equals(action)) {
                Log.d(TAG, "action: " + action + ", package: " + packageName);
                packageCounters.remove(packageName);
                if (preventPackages.containsKey(packageName)) {
                    Log.d(TAG, "force stop " + packageName + " if needed");
                    preventPackages.put(packageName, Boolean.TRUE);
                    forceStopPackageIfNeeded(packageName);
                }
            } else if (CommonIntent.ACTION_FORCE_STOP.equals(action)) {
                packageCounters.remove(packageName);
                if (preventPackages.containsKey(packageName)) {
                    preventPackages.put(packageName, Boolean.TRUE);
                }
                Log.d(TAG, "action: " + action + ", package: " + packageName + ", force stop later");
                forceStopPackageLater(packageName);
            }
        }
    }

    private static void loadPreventPackagesIfNeeded() {
        if (isSystemHook() && preventPackages == null) {
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

    public static HookResult hookIntentFilter$match(IntentFilter thiz, Object... args) {
        String action = (String) args[0];

        if (CommonIntent.ACTION_CHECK_HOOK.equals(action)) {
            return HookResult.HOOK_ENABLED;
        }

        if (IntentFilter.class.equals(thiz.getClass()) || HookIntentFilter.class.equals(thiz.getClass())) {
            return HookResult.None;
        }

        if (action != null && action.startsWith(CommonIntent.ACTION_NAMESPACE)) {
            return HookResult.None;
        }

        if (InputMethod.SERVICE_INTERFACE.equals(action)) {
            // input method
            return HookResult.None;
        } else if (AppWidgetManager.ACTION_APPWIDGET_UPDATE.equals(action)) {
            // app widget
            return HookResult.None;
        }

        // for BroadcastIntentFilter
        String packageName = (String) ReflectUtil.getObjectField(thiz, "packageName");
        if (packageName != null) {
            return disableBroadcastIntentIfNeeded(action, packageName);
        }

        // thiz.activity or thiz.service or thiz.provider
        // activity and receiver use the same ActivityIntentInfo
        Field field = ReflectUtil.getField(thiz.getClass(), "activity", "service", "provider");
        if (field == null) {
            return HookResult.None;
        }

        Object component;
        try {
            component = field.get(thiz);
        } catch (IllegalAccessException e) {
            return HookResult.None;
        }

        // component.owner
        Object owner = ReflectUtil.getObjectField(component, "owner");

        // component.owner.packageName
        packageName = (String) ReflectUtil.getObjectField(owner, "packageName");

        // component.owner.applicationInfo
        ApplicationInfo ai = (ApplicationInfo) ReflectUtil.getObjectField(owner, "applicationInfo");

        // component.owner.activities
        ArrayList<?> activities = (ArrayList<?>) ReflectUtil.getObjectField(owner, "activities");
        if (ai != null && (!packageUids.containsKey(packageName) || packageUids.get(packageName) != ai.uid)) {
            Log.d(TAG, "package: " + packageName + ", uid: " + ai.uid);
            packageUids.put(packageName, ai.uid);
        }

        if (activities != null && activities.contains(component)) {
            // we don't filter activities, otherwise cannot start activity
            return HookResult.None;
        }

        // component.owner.providers
        ArrayList<?> providers = (ArrayList<?>) ReflectUtil.getObjectField(owner, "providers");
        if (providers != null && providers.contains(component)) {
            // we don't filter providers
            return HookResult.None;
        }

        loadPreventPackagesIfNeeded();

        if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
            Log.v(TAG, "disallow " + component + ", action: " + action);
            return HookResult.NO_MATCH;
        }
        return HookResult.None;
    }

    public static boolean beforeActivityManagerService$startProcessLocked(Object[] args) {
        if (!isSystemHook()) {
            Log.e(SystemHook.TAG, "non-system call for ActivityManagerService$startProcessLocked");
            return true;
        }
        registerHookReceiverIfNeeded();
        ApplicationInfo info = (ApplicationInfo) ReflectUtil.getObjectField(args[0], "info");
        if (info == null) {
            Log.w(SystemHook.TAG, "no info field for " + args[0]);
            return true;
        }
        boolean disallow = "broadcast".equals(args[1]);
        loadPreventPackagesIfNeeded();
        if (disallow && Boolean.TRUE.equals(preventPackages.get(info.packageName))) {
            Field field = ReflectUtil.getField(args[0].getClass(), "pid");
            if (field != null) {
                try {
                    field.setInt(args[0], 0);
                } catch (IllegalAccessException e) {
                    Log.e(TAG, "field: " + field, e);
                }
            }
            forceStopPackageLaterIfPrevent(info.packageName);
            Log.d(SystemHook.TAG, "disallow start " + info.packageName + " for " + args[1] + " " + args[2]);
            return false;
        } else {
            Log.d(SystemHook.TAG, "allow start " + info.packageName + " for " + args[1] + " " + args[2]);
            return true;
        }
    }

    private static HookResult disableBroadcastIntentIfNeeded(String action, String packageName) {
        if (Intent.ACTION_CLOSE_SYSTEM_DIALOGS.equals(action)) {
            if (preventPackages.containsKey(packageName)) {
                Log.d(TAG, "disallow " + packageName + ", action: " + action);
                return HookResult.NO_MATCH;
            }
        }

        if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
            Log.v(TAG, "disallow " + packageName + ", action: " + action);
            Log.d(TAG, "force stop " + packageName + " to prevent broadcast");
            forceStopPackageLaterIfPrevent(packageName);
            return HookResult.NO_MATCH;
        }

        return HookResult.None;
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
                Log.d(TAG, "pid " + entry.getKey() + " is not for " + packageName);
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
        executor.schedule(new Runnable() {
            @Override
            public void run() {
                checkAndForceStopPackage(packageName);
            }
        }, 800, TimeUnit.MILLISECONDS);
    }

    private static void forceStopPackageLater(final String packageName) {
        executor.schedule(new Runnable() {
            @Override
            public void run() {
                int count = countCounter(packageName);
                if (count != 0) {
                    packageCounters.remove(packageName);
                    forceStopPackage(packageName);
                } else {
                    checkAndForceStopPackage(packageName);
                }
            }
        }, 800, TimeUnit.MILLISECONDS);
    }

    private static void forceStopPackageLaterIfPrevent(final String packageName) {
        executor.schedule(new Runnable() {
            @Override
            public void run() {
                if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
                    forceStopPackage(packageName);
                }
            }
        }, 800, TimeUnit.MILLISECONDS);
    }

    private static void checkAndForceStopPackage(String packageName) {
        for (ActivityManager.RunningServiceInfo service : getActivityManager().getRunningServices(Integer.MAX_VALUE)) {
            if (service.service.getPackageName().equals(packageName)) {
                Log.d(TAG, "package " + packageName + " has running services, force stop it");
                forceStopPackage(packageName);
                return;
            }
        }
        Log.d(TAG, "package " + packageName + " has no running services, ignore it");
        killNoFather(packageName);
    }

    private static ActivityManager getActivityManager() {
        if (activityManager == null) {
            activityManager = (ActivityManager) ActivityThread.currentApplication().getSystemService(Context.ACTIVITY_SERVICE);
        }
        return activityManager;
    }

    private static void forceStopPackage(final String packageName) {
        if ((preventPackages.containsKey(packageName))) {
            preventPackages.put(packageName, Boolean.TRUE);
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
                    Log.d(TAG, "kill " + pid + " without parent belongs to " + packageName);
//                } else if (isZombie(pid)) {
//                    Process.killProcess(pid);
//                    Log.d(TAG, "kill " + pid + " zombie belongs to " + packageName);
                }
            }
        }
    }


}
