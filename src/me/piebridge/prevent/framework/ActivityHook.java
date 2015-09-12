package me.piebridge.prevent.framework;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Process;

import me.piebridge.prevent.common.PreventIntent;

public class ActivityHook {

    private static ThreadLocal<Activity> context = new ThreadLocal<Activity>();

    private ActivityHook() {

    }

    public static String getPackageName(Activity activity) {
        return activity.getApplicationContext().getPackageName();
    }

    public static void hookBeforeOnCreate(Activity activity) {
        String packageName = getPackageName(activity);
        Intent intent = new Intent(PreventIntent.ACTION_INCREASE_COUNTER, Uri.fromParts(PreventIntent.SCHEME, packageName, null));
        intent.putExtra(PreventIntent.EXTRA_PID, Process.myPid());
        intent.putExtra(PreventIntent.EXTRA_UID, activity.getApplicationInfo().uid);
        sendBroadcast(activity, intent);
        context.set(activity);
    }

    public static void hookAfterOnDestroy(Activity activity) {
        String packageName = getPackageName(activity);
        Intent intent = new Intent(PreventIntent.ACTION_DECREASE_COUNTER, Uri.fromParts(PreventIntent.SCHEME, packageName, null));
        intent.putExtra(PreventIntent.EXTRA_PID, Process.myPid());
        sendBroadcast(activity, intent);
    }

    public static void hookBeforeOnRestart(Activity activity) {
        String packageName = getPackageName(activity);
        Intent intent = new Intent(PreventIntent.ACTION_RESTART, Uri.fromParts(PreventIntent.SCHEME, packageName, null));
        sendBroadcast(activity, intent);
    }

    public static void hookAfterMoveTaskToBack(Activity activity, Boolean result) {
        if (Boolean.TRUE.equals(result)) {
            PreventLog.d("move task to back: " + activity.getClass().getName());
            String packageName = getPackageName(activity);
            Intent intent = new Intent(PreventIntent.ACTION_ACTIVITY_DESTROY, Uri.fromParts(PreventIntent.SCHEME, packageName, null));
            sendBroadcast(activity, intent);
        }
    }

    public static void hookBeforeStartHomeActivityForResult(Activity activity) {
        PreventLog.d("start home activity: " + activity.getClass().getName());
        String packageName = getPackageName(activity);
        Intent intent = new Intent(PreventIntent.ACTION_ACTIVITY_DESTROY, Uri.fromParts(PreventIntent.SCHEME, packageName, null));
        sendBroadcast(activity, intent);
    }

    public static boolean stopSelf(int pid) {
        Activity activity = context.get();
        if (activity != null) {
            int uid = activity.getApplicationInfo().uid;
            if (pid != -1) {
                PreventLog.d("Process.killProcess(self) is called in activity, uid: " + uid);
            } else {
                PreventLog.d("System.exit is called in activity, uid: " + uid);
            }
            String packageName = getPackageName(activity);
            Intent intent = new Intent(PreventIntent.ACTION_FORCE_STOP, Uri.fromParts(PreventIntent.SCHEME, packageName, null));
            intent.putExtra(PreventIntent.EXTRA_UID, uid);
            intent.putExtra(PreventIntent.EXTRA_PID, Process.myPid());
            sendBroadcast(activity, intent);
        }
        context.remove();
        return false;
    }

    private static void sendBroadcast(Activity activity, Intent intent) {
        intent.addFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY);
        activity.sendBroadcast(intent);
    }

}
