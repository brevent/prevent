package me.piebridge.forcestopgb;

import android.app.ActivityManager;
import android.appwidget.AppWidgetManager;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.net.Uri;
import android.os.FileUtils;
import android.os.Handler;
import android.os.Process;
import android.text.TextUtils;
import android.util.Log;
import android.view.inputmethod.InputMethod;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

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

    private static Map<String, Boolean> preventPackages;

    private static Map<String, Boolean> systemPackages = new HashMap<String, Boolean>();

    private static Map<String, Integer> uids = new HashMap<String, Integer>();

    private static Map<String, HashMap<Integer, AtomicInteger>> packageCounters = new ConcurrentHashMap<String, HashMap<Integer, AtomicInteger>>();

    private static ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(2);

    public static Result hookIntentFilter$match(IntentFilter thiz, Object... args) {
        String action = (String) args[0];
        if (action == null) {
            return Result.None;
        }

        if (ACTION_HOOK.equals(action)) {
            PreventPackages.ensureDirectory();
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

        android.util.Log.v(TAG, "action: " + action + ", filter: " + thiz);

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
            return Result.None;
        }

        // component.owner.applicationInfo
        ApplicationInfo ai = (ApplicationInfo) getObjectField(owner, "applicationInfo");

        // component.owner.activities
        ArrayList<?> activities = (ArrayList<?>) getObjectField(owner, "activities");
        if (ai == null) {
            // do nothing
        } else if ((ai.flags & SYSTEM_APP) != 0) {
            // system package
            if (systemPackages.get(packageName) == null) {
                systemPackages.put(packageName, hasLauncher(activities));
            }
        } else {
            // normal package
            if (!uids.containsKey(packageName) || uids.get(packageName) != ai.uid) {
                Log.d(TAG, "package: " + packageName + ", uid: " + ai.uid);
                uids.put(packageName, ai.uid);
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

        return disableIntentIfNotRunning(action, packageName);
    }

    private static Result disableBroadcastIntentIfNeeded(String action, String packageName) {
        if (Intent.ACTION_CLOSE_SYSTEM_DIALOGS.equals(action)) {
            if (!systemPackages.containsKey(packageName)) {
                android.util.Log.d(TAG, "disallow " + packageName + ", action: " + action);
                return new Result(int.class, IntentFilter.NO_MATCH_ACTION);
            }
        }

        return disableIntentIfNotRunning(action, packageName);
    }

    private static Result disableIntentIfNotRunning(String action, String packageName) {
        if (Boolean.FALSE.equals(systemPackages.get(packageName))) {
            // we don't filter system package has no launcher
            return Result.None;
        }
        reloadPackagesIfNeeded();
        if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
            android.util.Log.v(TAG, "disallow " + packageName + ", action: " + action);
            return new Result(int.class, IntentFilter.NO_MATCH_ACTION);
        }
        return Result.None;
    }

    private static boolean isSystemHook() {
        return Process.myUid() == 1000;
    }

    private static boolean isSystemHook(String packageName) {
        return PACKAGE_NAME.equals(packageName) && isSystemHook();
    }

    private static int countCounter(String ssp) {
        HashMap<Integer, AtomicInteger> values = packageCounters.get(ssp);
        int count = 0;
        Iterator<Map.Entry<Integer, AtomicInteger>> iterator = values.entrySet().iterator();
        while (iterator.hasNext()) {
            Map.Entry<Integer, AtomicInteger> entry = iterator.next();
            if (checkPid(entry.getKey(), ssp)) {
                count += entry.getValue().get();
            } else {
                android.util.Log.d(TAG, "pid " + entry.getKey() + " is not for " + ssp);
                iterator.remove();
            }
        }
        return count;
    }

    private static String getPackage(int pid) {
        File file = new File(new File("/proc", String.valueOf(pid)), "cmdline");
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

    private static boolean checkPid(int pid, String ssp) {
        return ssp.equals(getPackage(pid));
    }

    private static boolean hookAsSystem(Object[] args) {
        String action = (String) args[0];
        Uri data = (Uri) args[3];
        String ssp = null;
        int pid = 0;
        int count = -1;
        String fragment = null;
        if (data != null) {
            fragment = data.getFragment();
            ssp = data.getSchemeSpecificPart();
            // max pid should be 65535
            if (fragment != null && fragment.length() < 6 && TextUtils.isDigitsOnly(fragment)) {
                pid = Integer.parseInt(fragment);
            }
            if (ssp != null && !packageCounters.containsKey(ssp)) {
                packageCounters.put(ssp, new HashMap<Integer, AtomicInteger>());
            }
        }
        if (ssp == null) {
            return false;
        } else if (Intent.ACTION_PACKAGE_RESTARTED.equals(action)) {
            count = 0;
            packageCounters.get(ssp).clear();
        } else if (ACTION_COUNTER_INCREASE.equals(action) && pid != 0) {
            if (!packageCounters.get(ssp).containsKey(pid)) {
                packageCounters.get(ssp).put(pid, new AtomicInteger());
            }
            packageCounters.get(ssp).get(pid).incrementAndGet();
            count = countCounter(ssp);
            reloadPackagesIfNeeded();
            if (Boolean.TRUE.equals(preventPackages.get(ssp))) {
                preventPackages.put(ssp, Boolean.FALSE);
                savePackages();
            }
        } else if (ACTION_COUNTER_DECREASE.equals(action) && pid != 0) {
            if (!packageCounters.get(ssp).containsKey(pid)) {
                packageCounters.get(ssp).put(pid, new AtomicInteger());
            } else {
                packageCounters.get(ssp).get(pid).decrementAndGet();
            }
            count = countCounter(ssp);
            reloadPackagesIfNeeded();
            if (count == 0 && preventPackages.containsKey(ssp)) {
                if (Boolean.FALSE.equals(preventPackages.get(ssp))) {
                    preventPackages.put(ssp, Boolean.TRUE);
                    savePackages();
                }
                forceStopPackageIfNeeded(ssp);
            }
        } else if (ADD_PREVENT_PACKAGE.equals(action)) {
            count = savePackage(ssp, true, fragment);
        } else if (REMOVE_PREVENT_PACKAGE.equals(action)) {
            count = savePackage(ssp, false, fragment);
        } else if (ACTION_FORCESTOP.equals(action)) {
            count = 0;
            forceStopPackageLater(ssp);
        } else if (ACTION_MOVE_TASK_TO_BACK.equals(action) || ACTION_START_HOME_ACTIVITY.equals(action)) {
            reloadPackagesIfNeeded();
            if (preventPackages.containsKey(ssp)) {
                count = 0;
                packageCounters.get(ssp).clear();
                forceStopPackageIfNeeded(ssp);
            } else {
                count = countCounter(ssp);
            }
        } else {
            return false;
        }
        android.util.Log.d(TAG, "action: " + action + ", package: " + ssp + ", counter: " + count);
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
                    android.util.Log.v(TAG, "activity " + activity + " is launcher");
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
        }, 800, TimeUnit.MICROSECONDS);
    }

    private static void forceStopPackageLater(final String packageName) {
        executor.schedule(new Runnable() {
            @Override
            public void run() {
                int count = countCounter(packageName);
                if (count != 0) {
                    packageCounters.get(packageName).clear();
                    forceStopPackage(packageName);
                }
            }
        }, 800, TimeUnit.MICROSECONDS);
    }

    private static void checkAndForceStopPackage(String packageName) {
        for (ActivityManager.RunningServiceInfo service : newActivityManagerIfNeeded().getRunningServices(Integer.MAX_VALUE)) {
            if (service.service.getPackageName().equals(packageName)) {
                forceStopPackage(packageName);
                return;
            }
        }
        killNoFather(packageName);
    }

    private static ActivityManager activityManager;

    private static ActivityManager newActivityManagerIfNeeded() {
        if (activityManager != null) {
            return activityManager;
        }
        try {
            Constructor<?> constructor = ActivityManager.class.getDeclaredConstructor(Context.class, Handler.class);
            constructor.setAccessible(true);
            activityManager = (ActivityManager) constructor.newInstance(null, null);
        } catch (Exception e) {
            android.util.Log.e(TAG, "cannot set new activityManager", e);
        }
        return activityManager;
    }

    private static void forceStopPackage(final String packageName) {
        reloadPackagesIfNeeded();
        if (Boolean.FALSE.equals(preventPackages.get(packageName))) {
            preventPackages.put(packageName, Boolean.TRUE);
            savePackages();
        }
        forceStopPackageNative(packageName);
        killNoFather(packageName);
    }

    private static void forceStopPackageNative(String packageName) {
        try {
            Method method = ActivityManager.class.getDeclaredMethod("forceStopPackage", String.class);
            method.setAccessible(true);
            method.invoke(newActivityManagerIfNeeded(), packageName);
            android.util.Log.i(TAG, "finish forceStopPackage " + packageName);
        } catch (RuntimeException e) {
            Log.e(TAG, "forceStopPackage", e);
        } catch (Exception e) {
            Log.e(TAG, "forceStopPackage", e);
        }
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

    public static void initPackages() {
        android.util.Log.d(TAG, "initPackages");
        boolean changed = false;
        Map<String, Boolean> packages = PreventPackages.load();
        for (String key : packages.keySet()) {
            if (!packages.get(key)) {
                changed = true;
                packages.put(key, Boolean.TRUE);
            }
        }
        if (changed) {
            PreventPackages.save(packages);
        }
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
        Integer uid = uids.get(packageName);
        if (uid == null) {
            return false;
        } else {
            try {
                killNoFather(uid);
            } catch (Throwable t) {
                Log.d(TAG, "cannot killNoFather for " + uid, t);
            }
            return true;
        }
    }

    private static void killNoFather(int uid) {
        File proc = new File("/proc");
        for (File file : proc.listFiles()) {
            if (file.isDirectory() && TextUtils.isDigitsOnly(file.getName())) {
                int pid = Integer.parseInt(file.getName());
                if (FileUtils.getUid(file.getAbsolutePath()) == uid && Process.getParentPid(pid) == 1) {
                    Process.killProcess(pid);
                    Log.d(TAG, "kill " + pid + " without parent");
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
                Log.e(TAG, "field: " + field);
                throw new IllegalAccessError(e.getMessage());
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
