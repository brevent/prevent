package me.piebridge.billing;

import android.text.TextUtils;

/**
 * Created by thom on 15/10/11.
 */
public class DonateUtils {

    public static final int REQUEST_CODE = 0x1000;

    public static final int API_VERSION = 3;

    public static final String ITEM_ID = "donate";

    public static final String ITEM_TYPE = "inapp";

    private DonateUtils() {

    }

    public static boolean isSignature(String signature) {
        return !TextUtils.isEmpty(signature);
    }

}
