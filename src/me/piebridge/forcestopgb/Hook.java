package me.piebridge.forcestopgb;

import android.app.Activity;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.Uri;
import android.os.Process;
import android.util.Log;

import java.util.Map;

public class Hook extends SystemHook {

    private static ThreadLocal<Activity> context = new ThreadLocal<Activity>();

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
            if (pid != -1) {
                Log.w(TAG, "Process.killProcess(self) is called in activity");
            } else {
                Log.w(TAG, "System.exit is called in activity");
            }
            String packageName = activity.getPackageName();
            activity.sendBroadcast(new Intent(ACTION_FORCESTOP, Uri.fromParts("package", packageName, null)));
        }
        context.remove();
        return false;
    }

    public static void afterActivity$moveTaskToBack(Activity thiz, Boolean result) {
        Intent intent = thiz.getIntent();
        android.util.Log.d(TAG, "moveTaskToBack: " + intent + ", result: " + result);
        if (Boolean.TRUE.equals(result)) {
            android.util.Log.d(TAG, "after moveTaskToBack: " + intent);
            String packageName = thiz.getPackageName();
            thiz.sendBroadcast(new Intent(ACTION_MOVE_TASK_TO_BACK, Uri.fromParts("package", packageName, null)));
        }
    }

    public static void beforeActivity$startHomeActivityForResult(Activity thiz) {
        String packageName = thiz.getPackageName();
        thiz.sendBroadcast(new Intent(ACTION_START_HOME_ACTIVITY, Uri.fromParts("package", packageName, null)));
    }
}
