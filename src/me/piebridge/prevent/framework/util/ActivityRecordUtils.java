package me.piebridge.prevent.framework.util;

import android.content.pm.ActivityInfo;
import android.content.pm.ApplicationInfo;

import java.lang.ref.WeakReference;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;

import me.piebridge.prevent.framework.PreventLog;

/**
 * Created by thom on 15/9/18.
 */
public class ActivityRecordUtils {

    private static Field weakActivity;
    private static Map<String, Field> fields = new HashMap<String, Field>();

    private ActivityRecordUtils() {

    }

    public static boolean isActivityRecord(Object object) {
        return object != null && object.getClass().getSimpleName().endsWith("ActivityRecord");
    }

    private static Object getField(Object target, String name) {
        Object activityRecord = getActivityRecord(target);
        Field field = fields.get(name);
        if (activityRecord != null && field == null) {
            try {
                field = activityRecord.getClass().getDeclaredField(name);
            } catch (NoSuchFieldException e) {
                PreventLog.v("cannot find field " + name + " in " + activityRecord, e);
                return null;
            }
            field.setAccessible(true);
            fields.put(name, field);
        }
        try {
            return field.get(activityRecord);
        } catch (IllegalAccessException e) {
            PreventLog.v("cannot access " + name + " in " + activityRecord, e);
            return null;
        }
    }

    public static Object getActivityRecord(Object target) {
        if (isActivityRecord(target)) {
            return target;
        }
        if (weakActivity == null) {
            try {
                weakActivity = target.getClass().getDeclaredField("weakActivity");
                weakActivity.setAccessible(true);
            } catch (NoSuchFieldException e) {
                PreventLog.d("cannot find weakActivity in " + target, e);
            }
        }
        try {
            return ((WeakReference<?>) weakActivity.get(target)).get();
        } catch (IllegalAccessException e) {
            PreventLog.v("cannot access " + weakActivity + " in " + target, e);
            return null;
        }
    }

    public static Object getTask(Object target) {
        return getField(target, "task");
    }

    public static String getPackageName(Object target) {
        return (String) getField(target, "packageName");
    }

    public static ActivityInfo getInfo(Object target) {
        return (ActivityInfo) getField(target, "info");
    }

    public static int getPid(Object target) {
        Object processRecord = getField(target, "app");
        return ProcessRecordUtils.getPid(processRecord);
    }

    public static int getUid(Object target) {
        Object processRecord = getField(target, "app");
        ApplicationInfo info = ProcessRecordUtils.getInfo(processRecord);
        if (info == null) {
            return 0;
        } else {
            return info.uid;
        }
    }

}
