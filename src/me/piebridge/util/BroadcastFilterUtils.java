package me.piebridge.util;

import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.util.Log;

import java.lang.reflect.Field;

import me.piebridge.forcestopgb.common.CommonIntent;
import me.piebridge.forcestopgb.hook.SystemHook;

/**
 * Created by thom on 15/7/14.
 */
public class BroadcastFilterUtils {

    private static Class<?> BroadcastFilter;
    private static Class<?> ReceiverList;
    private static Field BroadcastFilter$receiverList;
    private static Field ReceiverList$app;

    private BroadcastFilterUtils() {

    }

    static {
        initReflections();
    }

    private static void initReflections() {
        Log.d(CommonIntent.TAG, "init BroadcastFilterUtils");
        ClassLoader classLoader = SystemHook.getClassLoader();
        try {
            BroadcastFilter = Class.forName("com.android.server.am.BroadcastFilter", false, classLoader);
            BroadcastFilter$receiverList = BroadcastFilter.getDeclaredField("receiverList");
            BroadcastFilter$receiverList.setAccessible(true);

            ReceiverList = Class.forName("com.android.server.am.ReceiverList", false, classLoader);
            ReceiverList$app = ReceiverList.getDeclaredField("app");
            ReceiverList$app.setAccessible(true);
        } catch (ClassNotFoundException e) {
            Log.e(CommonIntent.TAG, "cannot find classes for BroadcastFilterUtils", e);
        } catch (NoSuchFieldException e) {
            Log.e(CommonIntent.TAG, "cannot find fields for BroadcastFilterUtils", e);
        }
    }

    public static String getPackageName(Object filter) {
        if (ReceiverList$app == null || !BroadcastFilter.isAssignableFrom(filter.getClass())) {
            return null;
        }
        try {
            Object receiverList = BroadcastFilter$receiverList.get(filter);
            Object app = ReceiverList$app.get(receiverList);
            ApplicationInfo info = ProcessRecordUtils.getInfo(app);
            if (info != null) {
                return info.packageName;
            }
        } catch (IllegalAccessException e) {
            Log.e(CommonIntent.TAG, "cannot get packageName from " + filter, e);
        }
        return null;
    }

}
