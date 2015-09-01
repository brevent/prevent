package me.piebridge.prevent.common;

import android.content.pm.PackageManager;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.ui.UILog;

/**
 * Created by byte on 15/9/1.
 */
public class SignatureUtils {

    private static Boolean signatureWorked;

    private static final String APPLICATION_ID = BuildConfig.APPLICATION_ID;

    private SignatureUtils() {

    }

    public static boolean canTrustSignature(PackageManager pm) {
        if (signatureWorked == null) {
            signatureWorked = !PackageUtils.isSystemSignaturePackage(pm, APPLICATION_ID);
            UILog.d("signatureWorked: " + signatureWorked);
        }
        return signatureWorked;
    }

}
