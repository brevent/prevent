package me.piebridge.prevent.framework;

import android.util.Log;

import de.robv.android.xposed.XposedBridge;

/**
 * Created by thom on 15/7/25.
 */
public class PreventLog {

    public static final String TAG = "Prevent";

    private static boolean hasXposedString = true;

    private static boolean hasXposedThrowable = true;

    private PreventLog() {

    }

    public static void v(String msg) {
        Log.v(TAG, msg);
    }

    public static void v(String msg, Throwable t) {
        Log.v(TAG, msg, t);
    }

    public static void d(String msg) {
        Log.d(TAG, msg);
        logToXposed("[D/" + TAG + "] " + msg);
    }

    public static void d(String msg, Throwable t) {
        Log.d(TAG, msg, t);
        logToXposed("[D/" + TAG + "] " + msg);
    }

    public static void i(String msg) {
        Log.i(TAG, msg);
        logToXposed("[I/" + TAG + "] " + msg);
    }

    public static void w(String msg) {
        Log.w(TAG, msg);
        logToXposed("[W/" + TAG + "] " + msg);
    }

    public static void e(String msg) {
        Log.e(TAG, msg);
        logToXposed("[E/" + TAG + "] " + msg);
    }

    public static void e(String msg, Throwable t) {
        Log.e(TAG, msg, t);
        logToXposed("[E/" + TAG + "] " + msg);
        logToXposed(t);
    }

    private static void logToXposed(String msg) {
        if (!hasXposedString) {
            return;
        }
        try {
            XposedBridge.log(msg);
        } catch (NoSuchMethodError e) {
            hasXposedString = false;
            Log.e(TAG, "no XposedBridge.log(String)", e);
        }
    }

    private static void logToXposed(Throwable t) {
        if (!hasXposedThrowable) {
            return;
        }
        try {
            XposedBridge.log(t);
        } catch (NoSuchMethodError e) {
            hasXposedThrowable = false;
            Log.e(TAG, "no XposedBridge.log(Throwable)", e);
        }
    }

}
