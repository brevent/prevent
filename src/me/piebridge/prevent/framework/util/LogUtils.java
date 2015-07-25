package me.piebridge.prevent.framework.util;

import android.os.Binder;

import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.framework.PreventLog;

/**
 * Created by thom on 15/7/23.
 */
public class LogUtils {

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
        PreventLog.i(sb.toString());
    }

    public static void logForceStop(String action, String packageName, String message) {
        StringBuilder sb = new StringBuilder();
        sb.append(ACTION);
        sb.append(action.replaceFirst(PreventIntent.NAMESPACE, ""));
        sb.append(", will force stop ");
        sb.append(packageName);
        sb.append(" ");
        sb.append(message);
        PreventLog.d(sb.toString());
    }

    public static void logIgnore(int key, String packageName) {
        StringBuilder sb = new StringBuilder();
        sb.append("pid ");
        sb.append(key);
        sb.append(" is not for ");
        sb.append(packageName);
        PreventLog.d(sb.toString());
    }

    public static void logRequest(String action, String packageName, int count) {
        StringBuilder sb = new StringBuilder();
        sb.append(ACTION);
        sb.append(action.replaceFirst(PreventIntent.NAMESPACE, ""));
        if (packageName != null) {
            sb.append(", ");
            sb.append(PACKAGE);
            sb.append(packageName);
        }
        if (count >= 0 && packageName != null) {
            sb.append(", count: ");
            sb.append(count);
            PreventLog.v(sb.toString());
        } else {
            PreventLog.d(sb.toString());
        }
    }

    public static void logIntentFilter(boolean disallow, final Object filter, final String action, final String packageName) {
        StringBuilder sb = new StringBuilder();
        sb.append(disallow ? "disallow" : "allow");
        sb.append(" ");
        sb.append(ACTION);
        sb.append(action.replaceFirst(PreventIntent.NAMESPACE, ""));
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
        PreventLog.v(sb.toString());
    }

    public static void logStartProcess(boolean disallow, final String packageName, final String hostingType, final Object hostingName) {
        StringBuilder sb = new StringBuilder();
        sb.append(disallow ? "disallow" : "wont disallow");
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
        if (disallow) {
            PreventLog.v(sb.toString());
        } else {
            PreventLog.d(sb.toString());
        }
    }

    public static void logIntentFilter(String action, Object filter, String packageName) {
        StringBuilder sb = new StringBuilder();
        sb.append(ACTION);
        sb.append(action.replaceFirst(PreventIntent.NAMESPACE, ""));
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
        PreventLog.v(sb.toString());
    }

}
