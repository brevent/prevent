package me.piebridge.forcestopgb.hook;

import android.app.Activity;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.Uri;
import android.os.Process;
import android.util.Log;

import me.piebridge.forcestopgb.common.CommonIntent;

public class Hook {

    private static ThreadLocal<Activity> context = new ThreadLocal<Activity>();

    public static void beforeActivity$onCreate(Activity thiz) {
        String packageName = thiz.getPackageName();
        Intent intent = new Intent(CommonIntent.ACTION_INCREASE_COUNTER, Uri.fromParts("package", packageName, null));
        intent.putExtra(CommonIntent.EXTRA_PID, Process.myPid());
        intent.putExtra(CommonIntent.EXTRA_UID, thiz.getApplicationInfo().uid);
        thiz.sendBroadcast(intent);
        context.set(thiz);
    }

    public static void afterActivity$onDestroy(Activity thiz) {
        String packageName = thiz.getPackageName();
        Intent intent = new Intent(CommonIntent.ACTION_DECREASE_COUNTER, Uri.fromParts("package", packageName, null));
        intent.putExtra(CommonIntent.EXTRA_PID, Process.myPid());
        thiz.sendBroadcast(intent);
    }

    public static boolean isHookEnabled() {
        return new IntentFilter().match(CommonIntent.ACTION_CHECK_HOOK, null, null, null, null, null) == CommonIntent.ACTION_HOOK_ENABLED;
    }

    public static boolean stopSelf(int pid) {
        Activity activity = context.get();
        if (activity != null) {
            if (pid != -1) {
                Log.w(CommonIntent.TAG, "Process.killProcess(self) is called in activity");
            } else {
                Log.w(CommonIntent.TAG, "System.exit is called in activity");
            }
            String packageName = activity.getPackageName();
            Intent intent = new Intent(CommonIntent.ACTION_FORCE_STOP, Uri.fromParts("package", packageName, null));
            activity.sendBroadcast(intent);
        }
        context.remove();
        return false;
    }

    public static void afterActivity$moveTaskToBack(Activity thiz, Boolean result) {
        if (Boolean.TRUE.equals(result)) {
            Log.d(CommonIntent.TAG, "moveTaskToBack: " + thiz.getClass());
            String packageName = thiz.getPackageName();
            Intent intent = new Intent(CommonIntent.ACTION_ACTIVITY_DESTROY, Uri.fromParts("package", packageName, null));
            thiz.sendBroadcast(intent);
        }
    }

    public static void beforeActivity$startHomeActivityForResult(Activity thiz) {
        Log.w(CommonIntent.TAG, "start home activity: " + thiz.getClass());
        String packageName = thiz.getPackageName();
        Intent intent = new Intent(CommonIntent.ACTION_ACTIVITY_DESTROY, Uri.fromParts("package", packageName, null));
        thiz.sendBroadcast(intent);
    }

}
