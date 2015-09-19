package me.piebridge.prevent.framework.util;

import android.os.Binder;

import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.framework.PreventLog;

/**
 * Created by thom on 15/7/23.
 */
public class LogUtils {

    private static final String SENDER = "sender: ";
    private static final String ACTION = "action: ";
    private static final String FILTER = "filter: ";
    private static final String PACKAGE = "package: ";
    private static final String DISALLOW = "disallow";
    private static final String ALLOW = "allow";

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
        PreventLog.v(sb.toString());
    }

    private static String buildLogRequest(String action, String packageName, int count) {
        StringBuilder sb = new StringBuilder();
        sb.append(ACTION);
        sb.append(action.replaceFirst(PreventIntent.NAMESPACE, ""));
        if (packageName != null) {
            sb.append(", ");
            sb.append(PACKAGE);
            sb.append(packageName);
        }
        if (count >= 0) {
            sb.append(", count: ");
            sb.append(count);
        }
        return sb.toString();
    }

    public static void logRequest(String action, String packageName, int count) {
        String log = buildLogRequest(action, packageName, count);
        PreventLog.d(log);
    }

    public static void logRequestInfo(String action, String packageName, int count) {
        String log = buildLogRequest(action, packageName, count);
        PreventLog.i(log);
    }

    private static String buildIntentFilterLog(boolean disallow, String sender, final Object filter, final String action, final String packageName) {
        StringBuilder sb = new StringBuilder();
        sb.append(disallow ? DISALLOW : ALLOW);
        sb.append(" ");
        sb.append(SENDER);
        sb.append(sender);
        sb.append(", ");
        sb.append(ACTION);
        sb.append(action.replaceFirst(PreventIntent.NAMESPACE, ""));
        if (packageName != null) {
            sb.append(", ");
            sb.append(PACKAGE);
            sb.append(packageName);
        }
        sb.append(", ");
        sb.append(FILTER);
        sb.append(filter);
        sb.append(", callingUid: ");
        sb.append(Binder.getCallingUid());
        sb.append(", callingPid: ");
        sb.append(Binder.getCallingPid());
        return sb.toString();
    }

    public static void logIntentFilter(boolean disallow, String sender, final Object filter, final String action, final String packageName) {
        PreventLog.v(buildIntentFilterLog(disallow, sender, filter, action, packageName));
    }

    public static void logIntentFilterInfo(boolean disallow, String sender, final Object filter, final String action, final String packageName) {
        PreventLog.i(buildIntentFilterLog(disallow, sender, filter, action, packageName));
    }

    public static void logStartProcess(final String packageName, final String hostingType, final Object hostingName, String sender) {
        logStartProcess(false, packageName, hostingType, hostingName, sender);
    }

    public static void logStartProcess(boolean disallow, final String packageName, final String hostingType, final Object hostingName, String sender) {
        StringBuilder sb = new StringBuilder();
        sb.append(disallow ? DISALLOW : ALLOW);
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
        sb.append(", sender=");
        sb.append(sender);
        if (disallow || "activity".equals(hostingType)) {
            PreventLog.d(sb.toString());
        } else {
            PreventLog.i(sb.toString());
        }
    }

    public static void logActivity(String action, String packageName, int count) {
        StringBuilder sb = new StringBuilder();
        sb.append(ACTION);
        sb.append(action);
        sb.append(", ");
        sb.append(PACKAGE);
        sb.append(packageName);
        sb.append(", count: ");
        sb.append(count);
        PreventLog.d(sb.toString());
    }

    public static void logActivity(String reason, Object activityRecord) {
        String packageName = ActivityRecordUtils.getPackageName(activityRecord);
        PreventLog.d(reason + ", callingPackage: " + packageName + ", ActivityRecord: " + activityRecord);
    }

}
