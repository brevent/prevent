package me.piebridge.prevent.framework.util;

import android.content.pm.ApplicationInfo;

import java.lang.reflect.Field;

import me.piebridge.prevent.framework.PreventLog;
import me.piebridge.prevent.framework.SystemHook;

/**
 * Created by thom on 15/7/14.
 */
public class ProcessRecordUtils {

    private static Class<?> ProcessRecord;

    private static Field ProcessRecord$info;

    private ProcessRecordUtils() {

    }

    static {
        initReflection();
    }

    public static void initReflection() {
        PreventLog.i("init ProcessRecordUtils");
        ClassLoader classLoader = SystemHook.getClassLoader();
        try {
            ProcessRecord = Class.forName("com.android.server.am.ProcessRecord", false, classLoader);
            ProcessRecord$info = ProcessRecord.getDeclaredField("info");
            ProcessRecord$info.setAccessible(true);
        } catch (ClassNotFoundException e) {
            PreventLog.e("cannot find class for ProcessRecordUtils", e);
        } catch (NoSuchFieldException e) {
            PreventLog.e("cannot find fields for ProcessRecordUtils", e);
        }
    }

    public static ApplicationInfo getInfo(Object pr) {
        if (pr == null || ProcessRecord$info == null || !ProcessRecord.isAssignableFrom(pr.getClass())) {
            return null;
        }
        try {
            return (ApplicationInfo) ProcessRecord$info.get(pr);
        } catch (IllegalAccessException e) {
            PreventLog.e("cannot get info", e);
            return null;
        }
    }

}
