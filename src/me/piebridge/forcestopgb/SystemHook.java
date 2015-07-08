package me.piebridge.forcestopgb;

import android.app.ActivityManager;
import android.appwidget.AppWidgetManager;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.net.Uri;
import android.os.Handler;
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

    private static long lastModified;

    private static Map<String, Boolean> preventPackages;

    private static Map<String, Boolean> systemPackages = new HashMap<String, Boolean>();

    private static Map<String, HashMap<Integer, AtomicInteger>> packageCounters = new HashMap<String, HashMap<Integer, AtomicInteger>>();

    private static ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(2);

    public static Result hookIntentFilter$match(IntentFilter thiz, Object... args) {
        String action = (String) args[0];
        if (ACTION_HOOK.equals(action)) {
            PreventPackages.ensureDirectory();
            return new Result(int.class, ACTION_HOOK_ENABLED);
        }

        if (InputMethod.SERVICE_INTERFACE.equals(action)) {
            // input method
            return Result.None;
        } else if (AppWidgetManager.ACTION_APPWIDGET_UPDATE.equals(action)) {
            // app widget
            return Result.None;
        }

        // thiz.activity or thiz.service or thiz.provider
        Field field = getField(thiz.getClass(), "service", "activity", "provider");
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
        String packageName = (String) getObjectField(owner, "packageName");

        if (action != null && action.startsWith(ACTION_PREFIX)) {
            if (isSystemHook(packageName)) {
                hookAsSystem(args);
            }
            return Result.None;
        }

        if (Intent.ACTION_PACKAGE_RESTARTED.equals(action) && isSystemHook(packageName)) {
            hookAsSystem(args);
        }

        // component.owner.activities
        ArrayList<?> activities = (ArrayList<?>) getObjectField(owner, "activities");
        // component.owner.applicationInfo
        ApplicationInfo applicationInfo = (ApplicationInfo) getObjectField(owner, "applicationInfo");
        if ((applicationInfo.flags & (ApplicationInfo.FLAG_SYSTEM | ApplicationInfo.FLAG_UPDATED_SYSTEM_APP)) != 0) {
            if (systemPackages.get(packageName) == null) {
                systemPackages.put(packageName, hasLauncher(activities));
            }
            // component.owner.providers
            ArrayList<?> providers = (ArrayList<?>) getObjectField(owner, "providers");
            if (providers.contains(component)) {
                // we don't filter providers for system package
                return Result.None;
            }
        }
        if (activities.contains(component)) {
            // we don't filter activities, otherwise, activity cannot be open directory
            return Result.None;
        }

        if (Boolean.FALSE.equals(systemPackages.get(packageName))) {
            // we don't filter system package has no launcher
            return Result.None;
        }

        reloadPackagesIfNeeded();
        if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
            android.util.Log.d(TAG, "disallow " + packageName + ", action: " + action);
            return new Result(int.class, IntentFilter.NO_MATCH_ACTION);
        }
        return Result.None;
    }

    private static boolean isSystemHook(String packageName) {
        return PACKAGE_NAME.equals(packageName) && android.os.Process.myUid() == 1000;
    }

    private static int countCounter(String ssp) {
        HashMap<Integer, AtomicInteger> values = packageCounters.get(ssp);
        int count = 0;
        Iterator<Map.Entry<Integer, AtomicInteger>> iterator = values.entrySet().iterator();
        while (iterator.hasNext()) {
            Map.Entry<Integer, AtomicInteger> entry = iterator.next();
            if (checkPid(entry.getKey(), ssp)) {
                android.util.Log.d(TAG, "pid: " + entry.getKey() + ", count: " + entry.getValue());
                count += entry.getValue().get();
            } else {
                android.util.Log.d(TAG, "pid " + entry.getKey() + " is not for " + ssp);
                iterator.remove();
            }
        }
        return count;
    }

    private static boolean checkPid(int pid, String ssp) {
        File file = new File(new File("/proc", String.valueOf(pid)), "cmdline");
        if (!file.isFile() || !file.canRead()) {
            return false;
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
            return os.toString().trim().equals(ssp);
        } catch (IOException e) {
            return true;
        }

    }

    private static void hookAsSystem(Object[] args) {
        String action = (String) args[0];
        Uri data = (Uri) args[3];
        String ssp = null;
        int pid = 0;
        int count = -1;
        if (data != null) {
            String fragment = data.getFragment();
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
        } else if (Intent.ACTION_PACKAGE_RESTARTED.equals(action)) {
            count = 0;
            packageCounters.get(ssp).clear();
        } else if (pid != 0 && ACTION_COUNTER_INCREASE.equals(action)) {
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
        } else if (pid != 0 && ACTION_COUNTER_DECREASE.equals(action)) {
            if (!packageCounters.get(ssp).containsKey(pid)) {
                packageCounters.get(ssp).put(pid, new AtomicInteger());
            } else {
                packageCounters.get(ssp).get(pid).decrementAndGet();
            }
            count = countCounter(ssp);
            reloadPackagesIfNeeded();
            if (count == 0 && Boolean.FALSE.equals(preventPackages.get(ssp))) {
                preventPackages.put(ssp, Boolean.TRUE);
                savePackages();
                forceStopPackageIfNeeded(ssp);
            }
        } else if (ACTION_FORCESTOP.equals(action)) {
            count = 0;
            final String stopPackageName = ssp;
            packageCounters.get(ssp).clear();
            executor.schedule(new Runnable() {
                @Override
                public void run() {
                    int counter = countCounter(stopPackageName);
                    Log.d(TAG, "force-stop " + stopPackageName + ", counter: " + counter);
                    if (counter == 0) {
                        forceStopPackage(stopPackageName);
                    }
                }
            }, 800, TimeUnit.MILLISECONDS);
        }
        android.util.Log.d(TAG, "action: " + action + ", package: " + ssp + ", counter: " + count);
    }

    private static boolean hasLauncher(ArrayList<?> activities) {
        for (Object activity : activities) {
            @SuppressWarnings("unchecked")
            ArrayList<? extends IntentFilter> intents = (ArrayList<? extends IntentFilter>) getObjectField(activity, "intents");
            for (IntentFilter intent : intents) {
                if (intent.hasAction(Intent.ACTION_MAIN) && (intent.hasCategory(Intent.CATEGORY_INFO) || intent.hasCategory(Intent.CATEGORY_LAUNCHER))) {
                    android.util.Log.d(TAG, "activity " + activity + " is launcher");
                    return true;
                }
            }
        }
        return false;
    }

    private static void forceStopPackageIfNeeded(String packageName) {
        for (ActivityManager.RunningServiceInfo service : newActivityManagerIfNeeded().getRunningServices(Integer.MAX_VALUE)) {
            if (service.service.getPackageName().equals(packageName)) {
                forceStopPackage(packageName);
                break;
            }
        }
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

    private static void forceStopPackage(String packageName) {
        try {
            Method method = ActivityManager.class.getDeclaredMethod("forceStopPackage", String.class);
            method.setAccessible(true);
            method.invoke(newActivityManagerIfNeeded(), packageName);
            android.util.Log.i(TAG, "forceStopPackage " + packageName);
        } catch (RuntimeException e) {
            Log.e(TAG, "forceStopPackage", e);
        } catch (Exception e) {
            Log.e(TAG, "forceStopPackage", e);
        }
        reloadPackagesIfNeeded();
        if (!preventPackages.containsKey(packageName)) {
            return;
        }
        if (Boolean.FALSE.equals(preventPackages.get(packageName))) {
            preventPackages.put(packageName, Boolean.TRUE);
            savePackages();
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

    private static Field getField(Class<?> clazz, boolean canPrivate, String name) {
        if (clazz == null) {
            return null;
        }
        try {
            final Field field = clazz.getDeclaredField(name);
            boolean isPrivate = Modifier.isPrivate(field.getModifiers());
            if (!isPrivate) {
                return field;
            }
            if (isPrivate && canPrivate) {
                field.setAccessible(true);
                return field;
            }
            if (canPrivate) {
                return clazz.getField(name);
            }
        } catch (NoSuchFieldException e) {
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
