package me.piebridge.util;

import android.os.Binder;
import android.util.Log;

import me.piebridge.forcestopgb.common.CommonIntent;

/**
 * Created by thom on 15/7/23.
 */
public class LogUtils {

    private static final String TAG = CommonIntent.TAG;
    private static final String ACTION = "action: ";
    private static final String FILTER = "filter: ";
    private static final String PACKAGE = "package: ";

    private LogUtils() {

    }

    public static void logKill(int pid, String reason, String packageName) {
        StringBuilder sb = new StringBuilder();
        sb.append("kill ");
        sb.append(pid);
        sb.append("(");
        sb.append(reason);
        sb.append("), ");
        sb.append(PACKAGE);
        sb.append(packageName);
        Log.d(TAG, sb.toString());
    }

    public static void logForceStop(String action, String packageName, String message) {
        StringBuilder sb = new StringBuilder();
        sb.append(ACTION);
        sb.append(action);
        sb.append(", force stop ");
        sb.append(packageName);
        sb.append(" ");
        sb.append(message);
        Log.d(TAG, sb.toString());
    }

    public static void logIgnore(int key, String packageName) {
        StringBuilder sb = new StringBuilder();
        sb.append("pid ");
        sb.append(key);
        sb.append(" is not for ");
        sb.append(packageName);
        Log.d(TAG, sb.toString());
    }

    public static void logRequest(String action, String packageName, int count) {
        StringBuilder sb = new StringBuilder();
        sb.append(ACTION);
        sb.append(action);
        sb.append(", ");
        sb.append(PACKAGE);
        sb.append(packageName);
        if (count >= 0) {
            sb.append(", count: ");
            sb.append(count);
        }
        Log.i(TAG, sb.toString());
    }

    public static void logIntentFilter(boolean disallow, final Object filter, final String action, final String packageName) {
        StringBuilder sb = new StringBuilder();
        sb.append(disallow ? "disallow" : "allow");
        sb.append(" ");
        sb.append(ACTION);
        sb.append(action);
        sb.append(", ");
        sb.append(FILTER);
        sb.append(filter);
        sb.append(", ");
        sb.append(PACKAGE);
        sb.append(packageName);
        sb.append(", callingUid: ");
        sb.append(Binder.getCallingUid());
        sb.append(", callingPid: ");
        sb.append(Binder.getCallingPid());
        if (disallow) {
            Log.v(TAG, sb.toString());
        } else {
            Log.d(TAG, sb.toString());
        }
    }

    public static void logStartProcess(final String allow, final String packageName, final String hostingType, final Object hostingName) {
        StringBuilder sb = new StringBuilder();
        sb.append(allow);
        sb.append(" start ");
        sb.append(packageName);
        sb.append(" for");
        if (hostingType != null) {
            sb.append(" ");
            sb.append(hostingType);
        }
        if (hostingName != null) {
            sb.append(" ");
            sb.append(hostingName);
        }
        Log.d(TAG, sb.toString());
    }


    public static void logIntentFilter(String action, Object filter, String packageName) {
        StringBuilder sb = new StringBuilder();
        sb.append(ACTION);
        sb.append(action);
        sb.append(", ");
        sb.append(FILTER);
        sb.append(filter);
        if (packageName != null) {
            sb.append(PACKAGE);
            sb.append(packageName);
        }
        sb.append(", callingUid: ");
        sb.append(Binder.getCallingUid());
        sb.append(", callingPid: ");
        sb.append(Binder.getCallingPid());
        Log.v(TAG, sb.toString());
    }

}
