package me.piebridge.forcestopgb.common;

import android.content.IntentFilter;

/**
 * Created by thom on 15/7/12.
 */
public final class CommonIntent {

    public static final String PACKAGE_NAME = "me.piebridge.forcestopgb";

    public static final String TAG = "PreventRunning";

    public static final String ACTION_NAMESPACE = "PREVENT_RUNNING.";

    // for hook check
    public static final int ACTION_HOOK_ENABLED = -IntentFilter.NO_MATCH_ACTION;
    public static final String ACTION_CHECK_HOOK = ACTION_NAMESPACE + ".CHECK_HOOK";

    // for hook
    public static final String ACTION_UPDATE_COUNTER = ACTION_NAMESPACE + "UPDATE_COUNTER";
    public static final String ACTION_ACTIVITY_DESTROY = ACTION_NAMESPACE + "ACTIVITY_DESTROY";
    public static final String ACTION_FORCE_STOP = ACTION_NAMESPACE + "FORCE_STOP";

    // for ui
    public static final String ACTION_GET_PACKAGES = ACTION_NAMESPACE + "GET_PACKAGES";
    public static final String ACTION_UPDATE_PREVENT = ACTION_NAMESPACE + "UPDATE_PREVENT";

    public static String EXTRA_PID = ACTION_NAMESPACE + "PID";
    public static String EXTRA_DELTA = ACTION_NAMESPACE + "DELTA";
    public static String EXTRA_PACKAGES = ACTION_NAMESPACE + "PACKAGES";
    public static String EXTRA_PREVENT = ACTION_NAMESPACE + "PREVENT";

    private CommonIntent() {

    }

}
