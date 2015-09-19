package me.piebridge.prevent.framework.util;

import android.content.ComponentName;
import android.content.Intent;

import java.lang.reflect.Field;

import me.piebridge.prevent.framework.PreventLog;

/**
 * Created by thom on 15/7/23.
 */
public class TaskRecordUtils {

    private static Field taskRecord$intent;

    private static Field taskRecord$affinityIntent;

    private TaskRecordUtils() {

    }

    public static String getPackageName(Object object) {
        try {
            Intent intent = getIntent(object);
            if (intent == null) {
                return null;
            }
            ComponentName cn = intent.getComponent();
            if (cn != null) {
                return cn.getPackageName();
            }
        } catch (NoSuchFieldException e) {
            PreventLog.e("cannot get field in TaskRecord", e);
        } catch (IllegalAccessException e) {
            PreventLog.e("cannot get field value in TaskRecord", e);
        }
        return null;
    }

    private static Intent getIntent(Object object) throws NoSuchFieldException, IllegalAccessException {
        Object taskRecord;
        if (ActivityRecordUtils.isActivityRecord(object)) {
            taskRecord = ActivityRecordUtils.getTask(object);
        } else {
            taskRecord = object;
        }
        if (taskRecord$intent == null) {
            taskRecord$intent = taskRecord.getClass().getDeclaredField("intent");
            taskRecord$intent.setAccessible(true);

            taskRecord$affinityIntent = taskRecord.getClass().getDeclaredField("affinityIntent");
            taskRecord$affinityIntent.setAccessible(true);
        }
        Intent intent = (Intent) taskRecord$intent.get(taskRecord);
        if (intent == null) {
            intent = (Intent) taskRecord$affinityIntent.get(taskRecord);
        }
        return intent;
    }

}
