package me.piebridge.prevent.framework;

import android.util.Log;

import de.robv.android.xposed.XposedBridge;

/**
 * Created by thom on 15/7/25.
 */
public class PreventLog {

    public static final String TAG = "Prevent";

    private PreventLog() {

    }

    public static void v(String msg) {
        Log.v(TAG, msg);
    }

    public static void d(String msg) {
        Log.d(TAG, msg);
        XposedBridge.log("[" + TAG + "]" + "[DEBUG]" + msg);
    }

    public static void i(String msg) {
        Log.i(TAG, msg);
        XposedBridge.log("[" + TAG + "]" + "[ INFO]" + msg);
    }

    public static void w(String msg) {
        Log.w(TAG, msg);
        XposedBridge.log("[" + TAG + "]" + "[ WARN]" + msg);
    }

    public static void e(String msg) {
        Log.e(TAG, msg);
        XposedBridge.log("[" + TAG + "]" + "[ERROR]" + msg);
    }

    public static void e(String msg, Throwable t) {
        Log.e(TAG, msg, t);
        XposedBridge.log("[" + TAG + "]" + "[ERROR]" + msg);
        XposedBridge.log(t);
    }

}
