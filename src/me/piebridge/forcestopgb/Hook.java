package me.piebridge.forcestopgb;

import android.app.Activity;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.Uri;
import android.os.Process;
import android.util.Log;

public class Hook {

    private static ThreadLocal<Activity> context = new ThreadLocal<Activity>();

    public static void beforeActivity$onCreate(Activity thiz, Object... args) {
        String packageName = thiz.getPackageName();
        String pid = String.valueOf(Process.myPid());
        thiz.sendBroadcast(newIntent(SystemHook.ACTION_COUNTER_INCREASE, packageName, pid));
        context.set(thiz);
    }

    public static void afterActivity$onDestroy(Activity thiz, Object... args) {
        String packageName = thiz.getPackageName();
        String pid = String.valueOf(Process.myPid());
        thiz.sendBroadcast(newIntent(SystemHook.ACTION_COUNTER_DECREASE, packageName, pid));
    }

    public static boolean isHookEnabled() {
        return new IntentFilter().match(SystemHook.ACTION_HOOK, null, null, null, null, null) == SystemHook.ACTION_HOOK_ENABLED;
    }

    public static boolean stopSelf(int pid) {
        Activity activity = context.get();
        if (activity != null) {
            if (pid != -1) {
                Log.w(SystemHook.TAG, "Process.killProcess(self) is called in activity");
            } else {
                Log.w(SystemHook.TAG, "System.exit is called in activity");
            }
            String packageName = activity.getPackageName();
            activity.sendBroadcast(newIntent(SystemHook.ACTION_FORCESTOP, packageName, null));
        }
        context.remove();
        return false;
    }

    public static void afterActivity$moveTaskToBack(Activity thiz, Boolean result) {
        Intent intent = thiz.getIntent();
        Log.d(SystemHook.TAG, "moveTaskToBack: " + intent + ", result: " + result);
        if (Boolean.TRUE.equals(result)) {
            String packageName = thiz.getPackageName();
            thiz.sendBroadcast(newIntent(SystemHook.ACTION_MOVE_TASK_TO_BACK, packageName, null));
        }
    }

    public static void beforeActivity$startHomeActivityForResult(Activity thiz, Intent intent) {
        Log.w(SystemHook.TAG, "call Home startActivityForResult: " + intent);
        String packageName = thiz.getPackageName();
        thiz.sendBroadcast(newIntent(SystemHook.ACTION_START_HOME_ACTIVITY, packageName, null));
    }

    public static Intent newIntent(String action, String packageName, String extra) {
        Intent intent = new Intent(action, Uri.fromParts("package", packageName, extra));
        intent.setPackage(SystemHook.PACKAGE_NAME);
        return intent;
    }

}
