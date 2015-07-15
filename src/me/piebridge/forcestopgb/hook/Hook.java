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

    private Hook() {

    }

    public static void beforeActivity$onCreate(Activity activity) { // NOSONAR
        String packageName = activity.getPackageName();
        Intent intent = new Intent(CommonIntent.ACTION_INCREASE_COUNTER, Uri.fromParts(CommonIntent.SCHEME, packageName, null));
        intent.putExtra(CommonIntent.EXTRA_PID, Process.myPid());
        intent.putExtra(CommonIntent.EXTRA_UID, activity.getApplicationInfo().uid);
        intent.setFlags(CommonIntent.INTENT_FLAG);
        activity.sendBroadcast(intent);
        context.set(activity);
    }

    public static void afterActivity$onDestroy(Activity activity) { // NOSONAR
        String packageName = activity.getPackageName();
        Intent intent = new Intent(CommonIntent.ACTION_DECREASE_COUNTER, Uri.fromParts(CommonIntent.SCHEME, packageName, null));
        intent.putExtra(CommonIntent.EXTRA_PID, Process.myPid());
        intent.setFlags(CommonIntent.INTENT_FLAG);
        activity.sendBroadcast(intent);
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
            Intent intent = new Intent(CommonIntent.ACTION_FORCE_STOP, Uri.fromParts(CommonIntent.SCHEME, packageName, null));
            intent.setFlags(CommonIntent.INTENT_FLAG);
            activity.sendBroadcast(intent);
        }
        context.remove();
        return false;
    }

    public static void afterActivity$moveTaskToBack(Activity activity, Boolean result) { // NOSONAR
        if (Boolean.TRUE.equals(result)) {
            Log.d(CommonIntent.TAG, "moveTaskToBack: " + activity.getClass());
            String packageName = activity.getPackageName();
            Intent intent = new Intent(CommonIntent.ACTION_ACTIVITY_DESTROY, Uri.fromParts(CommonIntent.SCHEME, packageName, null));
            intent.addFlags(CommonIntent.INTENT_FLAG);
            activity.sendBroadcast(intent);
        }
    }

    public static void beforeActivity$startHomeActivityForResult(Activity activity) { // NOSONAR
        Log.w(CommonIntent.TAG, "start home activity: " + activity.getClass());
        String packageName = activity.getPackageName();
        Intent intent = new Intent(CommonIntent.ACTION_ACTIVITY_DESTROY, Uri.fromParts(CommonIntent.SCHEME, packageName, null));
        intent.setFlags(CommonIntent.INTENT_FLAG);
        activity.sendBroadcast(intent);
    }

}
