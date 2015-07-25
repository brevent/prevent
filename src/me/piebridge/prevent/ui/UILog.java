package me.piebridge.prevent.ui;

import android.util.Log;

import de.robv.android.xposed.XposedBridge;

/**
 * Created by thom on 15/7/25.
 */
public class UILog {

    public static final String TAG = "PreventUI";

    private UILog() {

    }

    public static void d(String msg) {
        Log.d(TAG, msg);
        XposedBridge.log("[" + TAG + "]" + "[DEBUG]" + msg);
    }

    public static void d(String msg, Throwable t) {
        Log.d(TAG, msg, t);
        XposedBridge.log("[" + TAG + "]" + "[DEBUG]" + msg);
    }

    public static void i(String msg) {
        Log.i(TAG, msg);
        XposedBridge.log("[" + TAG + "]" + "[ INFO]" + msg);
    }

    public static void e(String msg, Throwable t) {
        Log.e(TAG, msg, t);
        XposedBridge.log("[" + TAG + "]" + "[ERROR]" + msg);
        XposedBridge.log(t);
    }

}
