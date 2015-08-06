package me.piebridge.prevent.framework.util;

import android.content.Context;
import android.os.ServiceManager;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import me.piebridge.prevent.framework.PreventLog;

/**
 * Created by thom on 15/8/2.
 */
public class AlarmManagerServiceUtils {

    private static Object alarmManagerService;
    private static Method removeLocked;

    private AlarmManagerServiceUtils() {

    }

    static {
        initMethod();
    }

    private static boolean initMethod() {
        PreventLog.i("init AlarmManagerServiceUtils");
        Object object = ServiceManager.getService(Context.ALARM_SERVICE);
        if (object == null) {
            PreventLog.d("cannot find service: " + Context.ALARM_SERVICE);
            return false;
        }
        PreventLog.d("AlarmManagerService: " + object.getClass().getName());
        if (!object.getClass().getName().contains("$")) {
            return initMethod(object);
        }
        return initMethod(HideApiUtils.getThis$0(object));
    }

    private static boolean initMethod(Object ams) {
        if (ams == null) {
            return false;
        }
        Class<?> clazz = ams.getClass();
        while (clazz != null) {
            for (Method method : clazz.getDeclaredMethods()) {
                Class<?>[] types = method.getParameterTypes();
                if ("removeLocked".equals(method.getName()) && types.length == 1 && types[0].equals(String.class)) {
                    method.setAccessible(true);
                    removeLocked = method;
                    alarmManagerService = ams;
                    PreventLog.d("find removeLocked in " + alarmManagerService.getClass().getName());
                    return true;
                }
            }
            clazz = clazz.getSuperclass();
        }
        PreventLog.d("cannot find removeLocked in " + ams);
        return false;
    }

    public static void releaseAlarm(String packageName) {
        if (removeLocked != null) {
            try {
                removeLocked.invoke(alarmManagerService, packageName);
                PreventLog.d("remove alarm lock for " + packageName);
            } catch (IllegalAccessException e) {
                PreventLog.d("cannot access removeLocked", e);
            } catch (InvocationTargetException e) {
                PreventLog.d("cannot invoke removeLocked", e);
            }
        }
    }

}
