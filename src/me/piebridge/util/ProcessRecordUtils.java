package me.piebridge.util;

import android.content.pm.ApplicationInfo;
import android.util.Log;

import java.lang.reflect.Field;

import me.piebridge.forcestopgb.common.CommonIntent;

/**
 * Created by thom on 15/7/14.
 */
public class ProcessRecordUtils {

    private static Class<?> ProcessRecord;

    private static Field ProcessRecord$info;

    private static Field ProcessRecord$pid;

    private ProcessRecordUtils() {

    }

    static {
        try {
            ProcessRecord = Class.forName("com.android.server.am.ProcessRecord");
            ProcessRecord$info = ProcessRecord.getDeclaredField("info");
            ProcessRecord$info.setAccessible(true);
            ProcessRecord$pid = ProcessRecord.getDeclaredField("pid");
            ProcessRecord$pid.setAccessible(true);
        } catch (ClassNotFoundException e) {
            Log.e(CommonIntent.TAG, "cannot find class for ProcessRecordUtils", e);
        } catch (NoSuchFieldException e) {
            Log.e(CommonIntent.TAG, "cannot find fields for ProcessRecordUtils", e);
        }
    }

    public static ApplicationInfo getInfo(Object pr) {
        if (pr == null || ProcessRecord$info == null || !ProcessRecord.isAssignableFrom(pr.getClass())) {
            return null;
        }
        try {
            return (ApplicationInfo) ProcessRecord$info.get(pr);
        } catch (IllegalAccessException e) {
            Log.e(CommonIntent.TAG, "cannot get info", e);
            return null;
        }
    }

    public static String getPackageName(Object pr) {
        ApplicationInfo info = getInfo(pr);
        if (info != null) {
            return info.packageName;
        } else {
            return null;
        }
    }

    public static int getPid(Object pr) {
        if (pr == null || ProcessRecord$pid == null || !ProcessRecord.isAssignableFrom(pr.getClass())) {
            return 0;
        }
        try {
            return (Integer) ProcessRecord$pid.get(pr);
        } catch (IllegalAccessException e) {
            Log.e(CommonIntent.TAG, "cannot get pid", e);
        }
        return 0;
    }

    public static void setPid(Object pr, int pid) {
        if (pr == null || ProcessRecord$pid == null || !ProcessRecord.isAssignableFrom(pr.getClass())) {
            return;
        }
        try {
            ProcessRecord$pid.setInt(pr, pid);
        } catch (IllegalAccessException e) {
            Log.e(CommonIntent.TAG, "cannot set pid", e);
        }
    }
}
