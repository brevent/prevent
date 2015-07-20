package me.piebridge.forcestopgb.common;

import android.content.Intent;

/**
 * Created by thom on 15/7/12.
 */
public final class CommonIntent {

    public static final String TAG = "PreventRunning";

    public static final String ACTION_NAMESPACE = "me.piebridge.forcestopgb.";

    // for hook
    public static final String ACTION_INCREASE_COUNTER = ACTION_NAMESPACE + "INCREASE_COUNTER";
    public static final String ACTION_DECREASE_COUNTER = ACTION_NAMESPACE + "DECREASE_COUNTER";
    public static final String ACTION_ACTIVITY_DESTROY = ACTION_NAMESPACE + "ACTIVITY_DESTROY";
    public static final String ACTION_FORCE_STOP = ACTION_NAMESPACE + "FORCE_STOP";
    public static final String ACTION_RESTART = ACTION_NAMESPACE + "RESTART";

    // for ui
    public static final String ACTION_GET_PACKAGES = ACTION_NAMESPACE + "GET_PACKAGES";
    public static final String ACTION_UPDATE_PREVENT = ACTION_NAMESPACE + "UPDATE_PREVENT";

    public static final String EXTRA_UID = ACTION_NAMESPACE + "UID";
    public static final String EXTRA_PID = ACTION_NAMESPACE + "PID";
    public static final String EXTRA_PACKAGES = ACTION_NAMESPACE + "PACKAGES";
    public static final String EXTRA_PREVENT = ACTION_NAMESPACE + "PREVENT";

    public static final int INTENT_FLAG = Intent.FLAG_RECEIVER_REGISTERED_ONLY;

    public static final String SCHEME = "prevent";

    private CommonIntent() {

    }

}
