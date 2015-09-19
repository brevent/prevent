package me.piebridge.prevent.common;

/**
 * Created by thom on 15/7/12.
 */
public final class PreventIntent {

    public static final String NAMESPACE = "me.piebridge.prevent.";

    // for ui - manager
    public static final String ACTION_GET_PACKAGES = NAMESPACE + "GET_PACKAGES";
    public static final String ACTION_GET_PROCESSES = NAMESPACE + "GET_PROCESSES";
    public static final String ACTION_UPDATE_PREVENT = NAMESPACE + "UPDATE_PREVENT";
    public static final String ACTION_REQUEST_LOG = NAMESPACE + "REQUEST_LOG";

    public static final String EXTRA_PACKAGES = NAMESPACE + "PACKAGES";
    public static final String EXTRA_PREVENT = NAMESPACE + "PREVENT";

    public static final String CATEGORY_ALARM = NAMESPACE + ".CATEGORY_ALARM";

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
