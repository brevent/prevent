package me.piebridge.prevent.common;

import android.content.pm.PackageManager;

/**
 * Created by thom on 15/7/28.
 */
public class GmsUtils {

    public static final String GMS = "com.google.android.gms";
    public static final String GAPPS_PREFIX = "com.google.android.";

    private GmsUtils() {

    }

    public static boolean isGapps(PackageManager pm, String packageName) {
        return pm.checkSignatures(packageName, GMS) == PackageManager.SIGNATURE_MATCH ||
                (packageName.startsWith(GAPPS_PREFIX) && pm.getLaunchIntentForPackage(GAPPS_PREFIX) != null);
    }

}
