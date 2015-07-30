package me.piebridge.prevent.framework.util;

import android.app.ActivityManager;
import android.app.Notification;
import android.content.Context;
import android.os.Process;
import android.os.ServiceManager;
import android.os.UserHandle;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import me.piebridge.prevent.framework.PreventLog;

/**
 * Created by thom on 15/7/12.
 */
public class HideApiUtils {

    private static final int REASON_PACKAGE_CHANGED = 5;

    private static final int[] REMOVE_FLAGS = new int[]{
            Notification.FLAG_FOREGROUND_SERVICE,
            Notification.FLAG_NO_CLEAR,
            Notification.FLAG_ONGOING_EVENT};

    private HideApiUtils() {

    }

    public static int getUidForPid(int pid) {
        return Process.getUidForPid(pid);
    }

    public static int getParentPid(int pid) {
        String[] procStatusLabels = {"PPid:"};
        long[] procStatusValues = new long[1];
        procStatusValues[0] = -1;
        Process.readProcLines("/proc/" + pid + "/status", procStatusLabels, procStatusValues);
        return (int) procStatusValues[0];
    }

    public static void forceStopPackage(ActivityManager activityManager, String packageName) {
        activityManager.forceStopPackage(packageName);
    }

    private static Method getMethod(Object notificationManagerService) {
        if (notificationManagerService == null) {
            return null;
        }
        Method cancelAllNotificationsInt = null;
        for (Method method : notificationManagerService.getClass().getDeclaredMethods()) {
            if ("cancelAllNotificationsInt".equals(method.getName())) {
                cancelAllNotificationsInt = method;
                break;
            }
        }
        if (cancelAllNotificationsInt != null) {
            cancelAllNotificationsInt.setAccessible(true);
        }
        return cancelAllNotificationsInt;
    }

    public static boolean cancelAllNotificationsInt(String pkgName) {
        Object notificationManagerService = ServiceManager.getService(Context.NOTIFICATION_SERVICE);
        Method cancelAllNotificationsInt = getMethod(notificationManagerService);
        if (cancelAllNotificationsInt == null) {
            return false;
        }

        int length = cancelAllNotificationsInt.getParameterTypes().length;
        try {
            for (int flag : REMOVE_FLAGS) {
                if (length == 0x5) {
                    cancelAllNotificationsInt.invoke(notificationManagerService, pkgName, flag, 0, true, UserHandle.USER_ALL);
                } else if (length == 0x9) {
                    cancelAllNotificationsInt.invoke(notificationManagerService, android.os.Process.myUid(), Process.myPid(), pkgName, flag, 0, true,
                            UserHandle.USER_ALL, REASON_PACKAGE_CHANGED, null);
                }
            }
        } catch (IllegalAccessException e) {
            PreventLog.d("cannot access cancelAllNotificationsInt", e);
        } catch (InvocationTargetException e) {
            PreventLog.d("cannot invoke cancelAllNotificationsInt", e);
        }

        return true;
    }

}