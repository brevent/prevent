package me.piebridge.prevent.framework.util;

import android.app.Notification;
import android.content.Context;
import android.os.*;
import android.os.Process;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import me.piebridge.prevent.framework.PreventLog;

/**
 * Created by thom on 15/7/31.
 */
public class NotificationManagerServiceUtils {

    private static Object notificationManagerService;

    private static Method cancelAllNotificationsInt;

    private static final int REASON_PACKAGE_CHANGED = 5;

    private static final int[] REMOVE_FLAGS = new int[]{
            Notification.FLAG_FOREGROUND_SERVICE,
            Notification.FLAG_NO_CLEAR,
            Notification.FLAG_ONGOING_EVENT};

    static {
        initMethod();
    }

    private NotificationManagerServiceUtils() {

    }

    private static boolean initMethod() {
        PreventLog.i("init NotificationManagerServiceUtils");
        Object object = ServiceManager.getService(Context.NOTIFICATION_SERVICE);
        if (object == null) {
            PreventLog.d("cannot find service: " + Context.NOTIFICATION_SERVICE);
            return false;
        }
        if (!object.getClass().getName().contains("$")) {
            return initMethod(object);
        }
        return initMethod(getThis$0(object));
    }

    private static boolean initMethod(Object nms) {
        if (nms == null) {
            return false;
        }
        PreventLog.d("notificationManagerService: " + nms);
        Class<?> clazz = nms.getClass();
        for (Method method : clazz.getDeclaredMethods()) {
            if ("cancelAllNotificationsInt".equals(method.getName())) {
                method.setAccessible(true);
                cancelAllNotificationsInt = method;
                notificationManagerService = nms;
                PreventLog.d("find cancelAllNotificationsInt in " + nms + ": " + cancelAllNotificationsInt);
                return true;
            }
        }
        PreventLog.d("cannot find cancelAllNotificationsInt in " + nms);
        return false;
    }

    private static Object getThis$0(Object object) { // NOSONAR
        Class<?> clazz = object.getClass();
        try {
            Field field = clazz.getDeclaredField("this$0");
            field.setAccessible(true);
            return field.get(object);
        } catch (NoSuchFieldException e) {
            PreventLog.d("cannot find this$0 in class: " + clazz, e);
        } catch (IllegalAccessException e) {
            PreventLog.d("cannot visit this$0 in class: " + clazz, e);
        }
        return null;
    }

    public static boolean cancelStickyNotification(String pkgName) {
        if (cancelAllNotificationsInt == null) {
            return false;
        }
        int uid = Process.myUid();
        int pid = Process.myPid();
        int length = cancelAllNotificationsInt.getParameterTypes().length;
        try {
            for (int flag : REMOVE_FLAGS) {
                if (length == 0x4) {
                    cancelAllNotificationsInt.invoke(notificationManagerService, pkgName, flag, 0, true);
                } else if (length == 0x5) {
                    cancelAllNotificationsInt.invoke(notificationManagerService, pkgName, flag, 0, true, UserHandle.USER_ALL);
                } else if (length == 0x9) {
                    cancelAllNotificationsInt.invoke(notificationManagerService, uid, pid, pkgName, flag, 0, true,
                            UserHandle.USER_ALL, REASON_PACKAGE_CHANGED, null);
                }
            }
            return true;
        } catch (IllegalAccessException e) {
            PreventLog.d("cannot access cancelAllNotificationsInt", e);
        } catch (InvocationTargetException e) {
            PreventLog.d("cannot invoke cancelAllNotificationsInt", e);
        }
        return false;
    }

}