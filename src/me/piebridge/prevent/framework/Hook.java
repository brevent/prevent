package me.piebridge.prevent.framework;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Process;

import me.piebridge.prevent.common.PreventIntent;

public class Hook {

    private static ThreadLocal<Activity> context = new ThreadLocal<Activity>();

    private Hook() {

    }

    public static void beforeActivity$onCreate(Activity activity) { // NOSONAR
        String packageName = activity.getPackageName();
        Intent intent = new Intent(PreventIntent.ACTION_INCREASE_COUNTER, Uri.fromParts(PreventIntent.SCHEME, packageName, null));
        intent.putExtra(PreventIntent.EXTRA_PID, Process.myPid());
        intent.putExtra(PreventIntent.EXTRA_UID, activity.getApplicationInfo().uid);
        sendBroadcast(activity, intent);
        context.set(activity);
    }

    public static void afterActivity$onDestroy(Activity activity) { // NOSONAR
        String packageName = activity.getPackageName();
        Intent intent = new Intent(PreventIntent.ACTION_DECREASE_COUNTER, Uri.fromParts(PreventIntent.SCHEME, packageName, null));
        intent.putExtra(PreventIntent.EXTRA_PID, Process.myPid());
        sendBroadcast(activity, intent);
    }

    public static void beforeActivity$onRestart(Activity activity) { // NOSONAR
        String packageName = activity.getPackageName();
        Intent intent = new Intent(PreventIntent.ACTION_RESTART, Uri.fromParts(PreventIntent.SCHEME, packageName, null));
        sendBroadcast(activity, intent);
    }

    public static boolean stopSelf(int pid) {
        Activity activity = context.get();
        if (activity != null) {
            int uid = activity.getApplicationInfo().uid;
            if (pid != -1) {
                PreventLog.i("Process.killProcess(self) is called in activity, uid: " + uid);
            } else {
                PreventLog.i("System.exit is called in activity, uid: " + uid);
            }
            String packageName = activity.getPackageName();
            Intent intent = new Intent(PreventIntent.ACTION_FORCE_STOP, Uri.fromParts(PreventIntent.SCHEME, packageName, null));
            intent.putExtra(PreventIntent.EXTRA_UID, uid);
            sendBroadcast(activity, intent);
        }
        context.remove();
        return false;
    }

    public static void afterActivity$moveTaskToBack(Activity activity, Boolean result) { // NOSONAR
        if (Boolean.TRUE.equals(result)) {
            PreventLog.i("moveTaskToBack: " + activity.getClass());
            String packageName = activity.getPackageName();
            Intent intent = new Intent(PreventIntent.ACTION_ACTIVITY_DESTROY, Uri.fromParts(PreventIntent.SCHEME, packageName, null));
            sendBroadcast(activity, intent);
        }
    }

    public static void beforeActivity$startHomeActivityForResult(Activity activity) { // NOSONAR
        PreventLog.i("start home activity: " + activity.getClass());
        String packageName = activity.getPackageName();
        Intent intent = new Intent(PreventIntent.ACTION_ACTIVITY_DESTROY, Uri.fromParts(PreventIntent.SCHEME, packageName, null));
        sendBroadcast(activity, intent);
    }

    private static void sendBroadcast(Activity activity, Intent intent) {
        intent.addFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY);
        activity.sendBroadcast(intent);
    }

}
