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
        XposedBridge.log("[D/" + TAG + "] " + msg);
    }

    public static void i(String msg) {
        Log.i(TAG, msg);
        XposedBridge.log("[I/" + TAG + "] " + msg);
    }

    public static void w(String msg) {
        Log.w(TAG, msg);
        XposedBridge.log("[W/" + TAG + "] " + msg);
    }

    public static void e(String msg) {
        Log.e(TAG, msg);
        XposedBridge.log("[E/" + TAG + "] " + msg);
    }

    public static void e(String msg, Throwable t) {
        Log.e(TAG, msg, t);
        XposedBridge.log("[E/" + TAG + "] " + msg);
        XposedBridge.log(t);
    }

}
