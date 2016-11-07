package me.piebridge.prevent.common;

import android.util.Log;

/**
 * Created by thom on 2016/11/7.
 */

public class CommonLog {

    public static final String TAG = "Prevent";

    private CommonLog() {

    }

    public static void v(String msg, Throwable t) {
        Log.v(TAG, msg, t);
    }

    public static void d(String msg) {
        Log.d(TAG, msg);
    }

    public static void i(String msg) {
        Log.i(TAG, msg);
    }

    public static void e(String msg, Throwable t) {
        Log.e(TAG, msg, t);
    }

}
