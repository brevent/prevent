package me.piebridge.forcestopgb;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import android.app.Activity;
import android.app.ActivityManager;
import android.app.ActivityManager.RunningServiceInfo;
import android.app.ActivityManagerNative;
import android.appwidget.AppWidgetManager;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.net.Uri;
import android.os.Build;
import android.os.RemoteException;
import android.os.UserHandle;
import android.os.Process;
import android.util.Log;
import android.view.inputmethod.InputMethod;

public class Hook {

    public static final String PACKAGE_NAME = "me.piebridge.forcestopgb";

    public static final String TAG = "FORCESTOP";

    private static final String ACTION_PREFIX = TAG;
    private static final String ACTION_HOOK = ACTION_PREFIX + ".HOOK";
    private static final int ACTION_HOOK_ENABLED = -IntentFilter.NO_MATCH_ACTION;
    private static final String ACTION_FORCESTOP = ACTION_PREFIX + ".FORCESTOP";
    private static final String ACTION_COUNTER_INCREASE = ACTION_PREFIX + ".COUNTER_INCREASE";
    private static final String ACTION_COUNTER_DECREASE = ACTION_PREFIX + ".COUNTER_DECREASE";

    public static final String ACTION_XPOSED_SECTION = "de.robv.android.xposed.installer.OPEN_SECTION";

    public static final int FLAG_ACTIVITY_LAUNCHER = Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_RESET_TASK_IF_NEEDED;

    private static long lastModified;

    private static Map<String, Boolean> preventPackages;

    private static Map<String, Boolean> systemPackages = new HashMap<String, Boolean>();

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

    private static ThreadLocal<AtomicInteger> counter = new ThreadLocal<AtomicInteger>() {
        @Override
        protected AtomicInteger initialValue() {
            return new AtomicInteger(0);
        }
    };

    private static ThreadLocal<Activity> context = new ThreadLocal<Activity>();

    public static void initPackages() {
        Map<String, Boolean> packages = PreventPackages.load();
        if (packages.containsValue(Boolean.FALSE)) {
            for (String key : packages.keySet()) {
                if (!packages.get(key)) {
                    packages.put(key, Boolean.TRUE);
                }
            }
            PreventPackages.save(packages);
        }
    }

    private static boolean isLauncher(Intent intent) {
        if (intent == null) {
            return false;
        }
        if (!Intent.ACTION_MAIN.equals(intent.getAction())) {
            return false;
        }
        Set<String> categories = intent.getCategories();
        if (categories == null) {
            return false;
        }
        if (!categories.contains(Intent.CATEGORY_INFO) && !categories.contains(Intent.CATEGORY_LAUNCHER)) {
            return false;
        }
        if ((intent.getFlags() & (Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_RESET_TASK_IF_NEEDED)) == 0) {
            return false;
        }
        Log.d(TAG, "intent " + intent + " is launched by launcher");
        return true;
    }

    public static void beforeActivity$onCreate(Activity thiz, Object... args) {
        int count = counter.get().incrementAndGet();
        Intent intent = thiz.getIntent();
        android.util.Log.d(TAG, "after onCreate: " + intent + ", count: " + count);
        reloadPackagesIfNeeded();
        String packageName = thiz.getPackageName();
        thiz.sendBroadcast(new Intent(ACTION_COUNTER_INCREASE, Uri.fromParts("package", packageName, isLauncher(intent) ? String.valueOf(count) : null)));
        if (!preventPackages.containsKey(packageName)) {
            return;
        }
        context.set(thiz);
        if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
            preventPackages.put(packageName, Boolean.FALSE);
            savePackages();
        }
    }

    public static void afterActivity$onDestroy(Activity thiz, Object... args) {
        int count = counter.get().decrementAndGet();
        Intent intent = thiz.getIntent();
        android.util.Log.d(TAG, "after onDestroy: " + intent + ", count: " + count);
        reloadPackagesIfNeeded();
        String packageName = thiz.getPackageName();
        thiz.sendBroadcast(new Intent(ACTION_COUNTER_DECREASE, Uri.fromParts("package", packageName, null)));
        if (!preventPackages.containsKey(packageName)) {
            return;
        }
        if (count == 0) {
            context.remove();
        }
        if (count == 0 && Boolean.FALSE.equals(preventPackages.get(packageName))) {
            preventPackages.put(packageName, Boolean.TRUE);
            savePackages();
            forceStopPackage(thiz, packageName);
        }
    }

    private static void forceStopPackage(Context context, String packageName) {
        final ActivityManager activityManager = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
        for (RunningServiceInfo service : activityManager.getRunningServices(Integer.MAX_VALUE)) {
            if (service.service.getPackageName().equals(packageName)) {
                context.sendBroadcast(getStopIntent(packageName));
                break;
            }
        }
    }

    private static void forceStopPackage(String packageName) {
        try {
            if (Build.VERSION.SDK_INT > Build.VERSION_CODES.JELLY_BEAN) {
                ActivityManagerNative.getDefault().forceStopPackage(packageName, UserHandle.myUserId());
            } else {
                ActivityManagerNative.getDefault().forceStopPackage(packageName);
            }
            android.util.Log.i(TAG, "forceStopPackage " + packageName);
        } catch (RuntimeException e) {
            Log.e(TAG, "forceStopPackage", e);
        } catch (RemoteException e) {
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

    private static Map<String, AtomicInteger> packageCounters = new HashMap<String, AtomicInteger>();

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
            if (!PACKAGE_NAME.equals(packageName) || Process.myUid() != 1000) {
                return Result.None;
            }
            Uri data = (Uri) args[3];
            String ssp = null;
            int count = -1;
            if (data != null) {
                ssp = data.getSchemeSpecificPart();
                if (ssp != null && !packageCounters.containsKey(ssp)) {
                    packageCounters.put(ssp, new AtomicInteger());
                }
            }
            if (ssp == null) {
            } else if (ACTION_COUNTER_INCREASE.equals(action)) {
                if (data.getFragment() != null) {
                    count = Integer.parseInt(data.getFragment());
                    packageCounters.get(ssp).set(count);
                } else {
                    count = packageCounters.get(ssp).incrementAndGet();
                }
            } else if (ACTION_COUNTER_DECREASE.equals(action)) {
                count = packageCounters.get(ssp).decrementAndGet();
            } else if (ACTION_FORCESTOP.equals(action)) {
                count = 0;
                final String stopPackageName = ssp;
                packageCounters.get(ssp).set(0);
                executor.schedule(new Runnable() {
                    @Override
                    public void run() {
                        Log.d(TAG, "force-stop " + stopPackageName + ", counter: " + packageCounters.get(stopPackageName).get());
                        if (packageCounters.get(stopPackageName).get() == 0) {
                            Log.i(TAG, "force-stop " + stopPackageName);
                            forceStopPackage(stopPackageName);
                        }
                    }
                }, 400, TimeUnit.MILLISECONDS);
            }
            android.util.Log.d(TAG, "action: " + action + ", counter: " + count);
            return Result.None;
        }

        // component.owner.applicationInfo
        ApplicationInfo applicationInfo = (ApplicationInfo) getObjectField(owner, "applicationInfo");

        // if the application is disabled, we do nothing
        if (!applicationInfo.enabled) {
            return Result.None;
        }

        // component.owner.providers
        ArrayList<?> providers = (ArrayList<?>) getObjectField(owner, "providers");
        if (providers.contains(component)) {
            // we don't filter providers
            return Result.None;
        }

        // component.owner.activities
        ArrayList<?> activities = (ArrayList<?>) getObjectField(owner, "activities");
        if ((applicationInfo.flags & (ApplicationInfo.FLAG_SYSTEM | ApplicationInfo.FLAG_UPDATED_SYSTEM_APP)) != 0) {
            if (systemPackages.get(packageName) == null) {
                systemPackages.put(packageName, hasLauncher(activities));
            }
        }
        if (activities.contains(component)) {
            // we don't filter activities
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

    public static boolean isHookEnabled() {
        return new IntentFilter().match(ACTION_HOOK, null, null, null, null, null) == ACTION_HOOK_ENABLED;
    }

    public static boolean stopSelf(int pid) {
        Activity activity = context.get();
        if (activity != null) {
            Log.w(TAG, "Process.killProcess(self) is called in activity");
            activity.sendBroadcast(getStopIntent(activity.getPackageName()));
            return false;
        } else {
            return false;
        }
    }

    private static Intent getStopIntent(String packageName) {
        Uri uri = Uri.fromParts("package", packageName, String.valueOf(System.currentTimeMillis()));
        return new Intent(ACTION_FORCESTOP, uri);
    }

    public static void forceStopActivity(Activity thiz) {
        reloadPackagesIfNeeded();
        String packageName = thiz.getPackageName();
        if (!preventPackages.containsKey(packageName)) {
            return;
        }
        context.remove();
        if (Boolean.FALSE.equals(preventPackages.get(packageName))) {
            preventPackages.put(packageName, Boolean.TRUE);
            savePackages();
            forceStopPackage(thiz, packageName);
        }
    }

    public static void afterActivity$moveTaskToBack(Activity thiz, Boolean result) {
        Intent intent = thiz.getIntent();
        android.util.Log.d(TAG, "moveTaskToBack: " + intent + ", result: " + result);
        if (Boolean.TRUE.equals(result)) {
            android.util.Log.d(TAG, "after moveTaskToBack: " + intent);
            forceStopActivity(thiz);
        }
    }

//	public static void beforeActivity$startActivityHome(Activity thiz, Intent intent) {
//		android.util.Log.d(TAG, "before startActivity: " + thiz.getIntent() + ", intent: " + intent);
//		forceStopActivity(thiz);
//	}
}
