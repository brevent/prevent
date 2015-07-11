package me.piebridge.forcestopgb;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
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
import android.appwidget.AppWidgetManager;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.net.Uri;
import android.os.Process;
import android.text.TextUtils;
import android.util.Log;
import android.view.inputmethod.InputMethod;

public class SystemHook {

    public static final String PACKAGE_NAME = "me.piebridge.forcestopgb";

    public static final String TAG = "FORCESTOP";

    private static final String ACTION_PREFIX = TAG;
    protected static final String ACTION_HOOK = ACTION_PREFIX + ".HOOK";
    protected static final int ACTION_HOOK_ENABLED = -IntentFilter.NO_MATCH_ACTION;
    protected static final String ACTION_FORCESTOP = ACTION_PREFIX + ".FORCESTOP";
    protected static final String ACTION_COUNTER_INCREASE = ACTION_PREFIX + ".COUNTER_INCREASE";
    protected static final String ACTION_COUNTER_DECREASE = ACTION_PREFIX + ".COUNTER_DECREASE";
    protected static final String ACTION_MOVE_TASK_TO_BACK = ACTION_PREFIX + ".MOVE_TASK_TO_BACK";
    protected static final String ACTION_START_HOME_ACTIVITY = ACTION_PREFIX + ".START_HOME_ACTIVITY";
    protected static final String REMOVE_PREVENT_PACKAGE = ACTION_PREFIX + ".REMOVE_PREVENT_PACKAGE";
    protected static final String ADD_PREVENT_PACKAGE = ACTION_PREFIX + ".ADD_PREVENT_PACKAGE";
    public static final String DELAY_SAVE = "delay";

    private static final int SYSTEM_APP = ApplicationInfo.FLAG_SYSTEM | ApplicationInfo.FLAG_UPDATED_SYSTEM_APP;

    private static long lastModified;

    private static ActivityManager activityManager;

    private static Map<String, Boolean> preventPackages;

    private static Map<String, Boolean> systemPackages = new HashMap<String, Boolean>();

    private static Map<String, Integer> packageUids = new HashMap<String, Integer>();

    private static Map<String, HashMap<Integer, AtomicInteger>> packageCounters = new ConcurrentHashMap<String, HashMap<Integer, AtomicInteger>>();

    private static ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(2);

    public static Result hookIntentFilter$match(IntentFilter thiz, Object... args) {
        String action = (String) args[0];
        if (action == null) {
            return Result.None;
        }

        if (ACTION_HOOK.equals(action)) {
            return new Result(int.class, ACTION_HOOK_ENABLED);
        }

        if (IntentFilter.class.equals(thiz.getClass())) {
            return Result.None;
        }

        if (action.startsWith(ACTION_PREFIX)) {
            if (isSystemHook()) {
                hookAsSystem(args);
            }
            return Result.None;
        }

        if (InputMethod.SERVICE_INTERFACE.equals(action)) {
            // input method
            return Result.None;
        } else if (AppWidgetManager.ACTION_APPWIDGET_UPDATE.equals(action)) {
            // app widget
            return Result.None;
        }

        // for BroadcastIntentFilter
        String packageName = (String) getObjectField(thiz, "packageName");
        if (packageName != null) {
            return disableBroadcastIntentIfNeeded(action, packageName);
        }

        // thiz.activity or thiz.service or thiz.provider
        // activity and receiver use the same ActivityIntentInfo
        Field field = getField(thiz.getClass(), "activity", "service", "provider");
        if (field == null) {
            Log.v(TAG, "not support action: " + action + ", filter: " + thiz);
            return Result.None;
        }

        Object component;
        try {
            component = field.get(thiz);
        } catch (IllegalAccessException e) {
            return Result.None;
        }

        // component.owner
        Object owner = getObjectField(component, "owner");

        // component.owner.packageName
        packageName = (String) getObjectField(owner, "packageName");

        if (Intent.ACTION_PACKAGE_RESTARTED.equals(action) && isSystemHook(packageName)) {
            hookAsSystem(args);
        }

        // component.owner.applicationInfo
        ApplicationInfo ai = (ApplicationInfo) getObjectField(owner, "applicationInfo");

        // component.owner.activities
        ArrayList<?> activities = (ArrayList<?>) getObjectField(owner, "activities");
        if (ai == null) {
            Log.w(TAG, "package " + packageName + " has no applicationInfo");
        } else {
            if ((ai.flags & SYSTEM_APP) != 0) {
                // system package
                if (systemPackages.get(packageName) == null) {
                    systemPackages.put(packageName, hasLauncher(activities));
                }
            }
            if (!packageUids.containsKey(packageName) || packageUids.get(packageName) != ai.uid) {
                Log.d(TAG, "package: " + packageName + ", uid: " + ai.uid);
                packageUids.put(packageName, ai.uid);
            }
        }

        if (activities != null && activities.contains(component)) {
            // we don't filter activities, otherwise cannot start activity
            return Result.None;
        }

        // component.owner.providers
        ArrayList<?> providers = (ArrayList<?>) getObjectField(owner, "providers");
        if (providers != null && providers.contains(component)) {
            // we don't filter providers
            return Result.None;
        }

        return disableIntentIfNotRunning(action, packageName, component.toString());
    }

    public static boolean beforeActivityManagerService$startProcessLocked(Object thiz, Object[] args) {
        if (!isSystemHook()) {
            Log.e(SystemHook.TAG, "non-system call for ActivityManagerService$startProcessLocked");
            return true;
        }
        ApplicationInfo info = (ApplicationInfo) SystemHook.getObjectField(args[0], "info");
        if (info == null) {
            Log.w(SystemHook.TAG, "no info field for " + args[0]);
            return true;
        }
        boolean disallow = "broadcast".equals(args[1]);
        if (disallow) {
            reloadPackagesIfNeeded();
        }
        if (disallow && Boolean.TRUE.equals(preventPackages.get(info.packageName))) {
            Field field = getField(args[0].getClass(), true, "pid");
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

    private static Result disableBroadcastIntentIfNeeded(String action, String packageName) {
        if (Intent.ACTION_CLOSE_SYSTEM_DIALOGS.equals(action)) {
            if (!systemPackages.containsKey(packageName)) {
                Log.d(TAG, "disallow " + packageName + ", action: " + action);
                return new Result(int.class, IntentFilter.NO_MATCH_ACTION);
            }
        }

        Result result = disableIntentIfNotRunning(action, packageName, packageName);
        if (!Result.None.equals(result)) {
            Log.d(TAG, "force stop " + packageName + " to prevent broadcast");
            forceStopPackageLaterIfPrevent(packageName);
        }
        return result;
    }

    private static Result disableIntentIfNotRunning(String action, String packageName, String content) {
        if (Boolean.FALSE.equals(systemPackages.get(packageName))) {
            // we don't filter system package has no launcher
            return Result.None;
        }
        reloadPackagesIfNeeded();
        if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
            Log.v(TAG, "disallow " + content + ", action: " + action);
            return new Result(int.class, IntentFilter.NO_MATCH_ACTION);
        }
        return Result.None;
    }

    private static boolean isSystemHook() {
        return Process.myUid() == Process.SYSTEM_UID;
    }

    private static boolean isSystemHook(String packageName) {
        return PACKAGE_NAME.equals(packageName) && isSystemHook();
    }

    private static int countCounter(String packageName) {
        HashMap<Integer, AtomicInteger> values = packageCounters.get(packageName);
        int count = 0;
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

    private static String getContent(File file) {
        if (!file.isFile() || !file.canRead()) {
            return null;
        }

        try {
            InputStream is = new BufferedInputStream(new FileInputStream(file));
            ByteArrayOutputStream os = new ByteArrayOutputStream();
            try {
                byte[] buffer = new byte[8192];
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
        return processName != null && uid != null && processName.startsWith(packageName) && Process.getUidForPid(pid) == uid;
    }

    private static boolean hookAsSystem(Object[] args) {
        String action = (String) args[0];
        Uri data = (Uri) args[3];
        String packageName = null;
        int pid = 0;
        int count;
        String fragment = null;
        if (data != null) {
            fragment = data.getFragment();
            packageName = data.getSchemeSpecificPart();
            // max pid should be 65535
            if (fragment != null && fragment.length() < 6 && TextUtils.isDigitsOnly(fragment)) {
                pid = Integer.parseInt(fragment);
            }
            if (packageName != null && !packageCounters.containsKey(packageName)) {
                packageCounters.put(packageName, new HashMap<Integer, AtomicInteger>());
            }
        }
        if (packageName == null) {
            return false;
        } else if (Intent.ACTION_PACKAGE_RESTARTED.equals(action)) {
            count = 0;
            packageCounters.get(packageName).clear();
            reloadPackagesIfNeeded();
            if (Boolean.FALSE.equals(preventPackages.get(packageName))) {
                preventPackages.put(packageName, Boolean.TRUE);
                savePackages();
            }
        } else if (ACTION_COUNTER_INCREASE.equals(action) && pid != 0) {
            if (!packageCounters.get(packageName).containsKey(pid)) {
                packageCounters.get(packageName).put(pid, new AtomicInteger());
            }
            packageCounters.get(packageName).get(pid).incrementAndGet();
            count = countCounter(packageName);
            reloadPackagesIfNeeded();
            if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
                preventPackages.put(packageName, Boolean.FALSE);
                savePackages();
            }
        } else if (ACTION_COUNTER_DECREASE.equals(action) && pid != 0) {
            if (!packageCounters.get(packageName).containsKey(pid)) {
                packageCounters.get(packageName).put(pid, new AtomicInteger());
            } else {
                packageCounters.get(packageName).get(pid).decrementAndGet();
            }
            count = countCounter(packageName);
            reloadPackagesIfNeeded();
            if (count == 0 && preventPackages.containsKey(packageName)) {
                if (Boolean.FALSE.equals(preventPackages.get(packageName))) {
                    preventPackages.put(packageName, Boolean.TRUE);
                    savePackages();
                }
                forceStopPackageIfNeeded(packageName);
            }
        } else if (ADD_PREVENT_PACKAGE.equals(action)) {
            count = savePackage(packageName, true, fragment);
        } else if (REMOVE_PREVENT_PACKAGE.equals(action)) {
            count = savePackage(packageName, false, fragment);
        } else if (ACTION_FORCESTOP.equals(action)) {
            count = countCounter(packageName);
            forceStopPackageLater(packageName);
        } else if (ACTION_MOVE_TASK_TO_BACK.equals(action) || ACTION_START_HOME_ACTIVITY.equals(action)) {
            count = countCounter(packageName);
            reloadPackagesIfNeeded();
            if (preventPackages.containsKey(packageName)) {
                packageCounters.get(packageName).clear();
                forceStopPackageIfNeeded(packageName);
            }
        } else {
            return false;
        }
        Log.d(TAG, "action: " + action + ", package: " + packageName + ", counter: " + count);
        return true;
    }

    private static boolean hasLauncher(ArrayList<?> activities) {
        for (Object activity : activities) {
            @SuppressWarnings("unchecked")
            ArrayList<? extends IntentFilter> intents = (ArrayList<? extends IntentFilter>) getObjectField(activity, "intents");
            if (intents == null) {
                return false;
            }
            for (IntentFilter intent : intents) {
                if (intent.hasAction(Intent.ACTION_MAIN) && (intent.hasCategory(Intent.CATEGORY_INFO) || intent.hasCategory(Intent.CATEGORY_LAUNCHER))) {
                    Log.v(TAG, "activity " + activity + " is launcher");
                    return true;
                }
            }
        }
        return false;
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
                    packageCounters.get(packageName).clear();
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
                reloadPackagesIfNeeded();
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
        reloadPackagesIfNeeded();
        if (Boolean.FALSE.equals(preventPackages.get(packageName))) {
            preventPackages.put(packageName, Boolean.TRUE);
            savePackages();
        }
        try {
            getActivityManager().forceStopPackage(packageName);
            Log.i(TAG, "finish force stop package " + packageName);
        } catch (Throwable t) {
            Log.e(TAG, "cannot force stop package" + packageName, t);
        }
        killNoFather(packageName);
    }

    private static void reloadPackagesIfNeeded() {
        long time = PreventPackages.lastModified();
        if (time > lastModified || preventPackages == null) {
            lastModified = time;
            preventPackages = PreventPackages.load();
        }
    }

    private static void savePackages() {
        lastModified = PreventPackages.save(preventPackages);
    }

    public static void resetPackages() {
        Log.d(TAG, "resetPackages");
        PreventPackages.resetPackages();
    }

    private static int savePackage(String packageName, boolean added, String fragment) {
        int count = countCounter(packageName);
        reloadPackagesIfNeeded();
        if (added && !preventPackages.containsKey(packageName)) {
            preventPackages.put(packageName, count == 0);
            if (!DELAY_SAVE.equals(fragment)) {
                savePackages();
            }
        } else if (!added && preventPackages.containsKey(packageName)) {
            preventPackages.remove(packageName);
            if (!DELAY_SAVE.equals(fragment)) {
                savePackages();
            }
        }
        return count;
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
                if (Process.getUidForPid(pid) != uid) {
                    continue;
                }
                if (Process.getParentPid(pid) == 1) {
                    Process.killProcess(pid);
                    Log.d(TAG, "kill " + pid + " without parent belongs to " + packageName);
//                } else if (isZombie(pid)) {
//                    Process.killProcess(pid);
//                    Log.d(TAG, "kill " + pid + " zombie belongs to " + packageName);
                }
            }
        }
    }

    private static Field getField(Class<?> clazz, boolean canPrivate, String name) {
        if (clazz == null) {
            return null;
        }
        try {
            final Field field = clazz.getDeclaredField(name);
            boolean isPrivate = Modifier.isPrivate(field.getModifiers());
            field.setAccessible(true);
            if (!isPrivate) {
                return field;
            }
            if (canPrivate) {
                return field;
            }
        } catch (NoSuchFieldException e) {
            // do nothing
        }
        return getField(clazz.getSuperclass(), false, name);
    }

    private static Field getField(Class<?> clazz, String name) {
        return getField(clazz, true, name);
    }

    private static Field getField(Class<?> clazz, String... names) {
        for (String name : names) {
            final Field field = getField(clazz, name);
            if (field != null) {
                return field;
            }
        }
        return null;
    }

    private static Object getObjectField(Object object, String name) {
        Field field = getField(object.getClass(), name);
        if (field != null) {
            try {
                return field.get(object);
            } catch (IllegalAccessException e) {
                Log.e(TAG, "field: " + field, e);
            }
        }
        return null;
    }

    public static class Result {
        public static final Result None = new Result();
        private Class<?> type;
        private Object result;

        private Result() {
            type = Void.class;
        }

        public Result(Object _result) {
            result = _result;
            if (result != null) {
                type = _result.getClass();
            }
        }

        public Result(Class<?> _type, Object _result) {
            type = _type;
            result = _result;
        }

        public boolean isNone() {
            return Void.class.equals(this.type);
        }

        public Object getResult() {
            return result;
        }
    }

}
