package me.piebridge.forcestopgb;

import android.app.Activity;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.Uri;
import android.os.Process;
import android.util.Log;

import java.util.Map;

public class Hook extends SystemHook {

    private static long lastModified;

    private static Map<String, Boolean> preventPackages;

    private static ThreadLocal<Activity> context = new ThreadLocal<Activity>();

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

    public static void beforeActivity$onCreate(Activity thiz, Object... args) {
        String packageName = thiz.getPackageName();
        String pid = String.valueOf(Process.myPid());
        thiz.sendBroadcast(new Intent(ACTION_COUNTER_INCREASE, Uri.fromParts("package", packageName, pid)));
        context.set(thiz);
    }

    public static void afterActivity$onDestroy(Activity thiz, Object... args) {
        String packageName = thiz.getPackageName();
        String pid = String.valueOf(Process.myPid());
        thiz.sendBroadcast(new Intent(ACTION_COUNTER_DECREASE, Uri.fromParts("package", packageName, pid)));
    }

    public static boolean isHookEnabled() {
        return new IntentFilter().match(ACTION_HOOK, null, null, null, null, null) == ACTION_HOOK_ENABLED;
    }

    public static boolean stopSelf(int pid) {
        Activity activity = context.get();
        if (activity != null) {
            Log.w(TAG, "Process.killProcess(self) is called in activity");
            forceStopActivityIfNeeded(activity);
        }
        context.remove();
        return false;
    }

    private static Intent getStopIntent(String packageName) {
        Uri uri = Uri.fromParts("package", packageName, String.valueOf(System.currentTimeMillis()));
        return new Intent(ACTION_FORCESTOP, uri);
    }

    public static void forceStopActivityIfNeeded(Activity thiz) {
        reloadPackagesIfNeeded();
        String packageName = thiz.getPackageName();
        if (!preventPackages.containsKey(packageName)) {
            return;
        }
        if (Boolean.FALSE.equals(preventPackages.get(packageName))) {
            preventPackages.put(packageName, Boolean.TRUE);
            savePackages();
            thiz.sendBroadcast(getStopIntent(thiz.getPackageName()));
        }
    }

    public static void afterActivity$moveTaskToBack(Activity thiz, Boolean result) {
        Intent intent = thiz.getIntent();
        android.util.Log.d(TAG, "moveTaskToBack: " + intent + ", result: " + result);
        if (Boolean.TRUE.equals(result)) {
            android.util.Log.d(TAG, "after moveTaskToBack: " + intent);
            forceStopActivityIfNeeded(thiz);
        }
    }
}
