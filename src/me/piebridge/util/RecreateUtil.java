package me.piebridge.util;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;

import android.annotation.TargetApi;
import android.app.Activity;
import android.content.res.Configuration;
import android.os.Build;
import android.os.IBinder;

public class RecreateUtil {

    private RecreateUtil() {

    }

    public static void recreate(Activity activity) {
        if (Build.VERSION.SDK_INT > Build.VERSION_CODES.GINGERBREAD_MR1) {
            recreateHC(activity);
        } else {
            try {
                recreateGB(activity);
            } catch (Throwable t) { // NOSONAR
                // do nothing
            }
        }
    }

    @TargetApi(Build.VERSION_CODES.HONEYCOMB)
    private static void recreateHC(Activity activity) {
        ((Activity) activity).recreate();
    }

    private static void recreateGB(Activity activity) throws IllegalArgumentException, IllegalAccessException, NoSuchMethodException, InvocationTargetException {
        Field Activity$mToken = ReflectUtil.getField(Activity.class, "mToken"); // NOSONAR
        IBinder mToken = (IBinder) Activity$mToken.get(activity);
        Field Activity$mMainThread = ReflectUtil.getField(Activity.class, "mMainThread"); // NOSONAR
        Object mMainThread = Activity$mMainThread.get(activity);
        Field ActivityThread$mAppThread = ReflectUtil.getField(mMainThread.getClass(), "mAppThread"); // NOSONAR
        Object mAppThread = ActivityThread$mAppThread.get(mMainThread);
        // @formatter:off
        Method method = mAppThread.getClass().getMethod("scheduleRelaunchActivity",
                IBinder.class, List.class, List.class, int.class, boolean.class, Configuration.class);
        // @formatter:on
        method.invoke(mAppThread, mToken, null, null, 0, false, null);
    }

}
