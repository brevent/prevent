package me.piebridge.prevent.common;

/**
 * Created by thom on 15/7/12.
 */
public final class PreventIntent {

    public static final String NAMESPACE = "me.piebridge.prevent.";

    // for hook
    public static final String ACTION_INCREASE_COUNTER = NAMESPACE + "INCREASE_COUNTER";
    public static final String ACTION_DECREASE_COUNTER = NAMESPACE + "DECREASE_COUNTER";
    public static final String ACTION_ACTIVITY_DESTROY = NAMESPACE + "ACTIVITY_DESTROY";
    public static final String ACTION_FORCE_STOP = NAMESPACE + "FORCE_STOP";
    public static final String ACTION_RESTART = NAMESPACE + "RESTART";

    // for ui - manager
    public static final String ACTION_GET_PACKAGES = NAMESPACE + "GET_PACKAGES";
    public static final String ACTION_GET_PROCESSES = NAMESPACE + "GET_PROCESSES";
    public static final String ACTION_UPDATE_PREVENT = NAMESPACE + "UPDATE_PREVENT";

    public static final String EXTRA_UID = NAMESPACE + "UID";
    public static final String EXTRA_PID = NAMESPACE + "PID";
    public static final String EXTRA_PACKAGES = NAMESPACE + "PACKAGES";
    public static final String EXTRA_PREVENT = NAMESPACE + "PREVENT";

    public static final String SCHEME = "prevent";
    public static final String PERMISSION_MANAGER = NAMESPACE + "permission.MANAGER";
    public static final String PERMISSION_SYSTEM = "android.permission.SHUTDOWN";

    public static final String NAME_ALIPAY = "com.eg.android.AlipayGphone";
    public static final String CLASS_ALIPAY = "com.alipay.mobile.mob.components.account.AccountCodeActivity_";

    public static final String NAME_WECHAT = "com.tencent.mm";
    public static final String CLASS_WECHAT = "com.tencent.mm.plugin.remittance.ui.RemittanceAdapterUI";

    private PreventIntent() {

    }

}
