package me.piebridge.prevent.common;

import android.content.Context;
import android.content.pm.PackageManager;

import java.util.Arrays;
import java.util.Collection;
import java.util.concurrent.atomic.AtomicInteger;

import me.piebridge.prevent.framework.PreventLog;

/**
 * Created by thom on 15/7/28.
 */
public class GmsUtils {

    public static final String GMS = "com.google.android.gms";
    public static final String GAPPS_PREFIX = "com.google.android.";
    private static final AtomicInteger GMS_COUNTER = new AtomicInteger();
    // https://developers.google.com/cloud-messaging/android/client
    private static Collection<String> GCM_ACTIONS = Arrays.asList(
            "com.google.android.c2dm.intent.RECEIVE",
            "com.google.android.c2dm.intent.REGISTRATION");

    private GmsUtils() {

    }

    public static boolean isGapps(PackageManager pm, String packageName) {
        return packageName.startsWith(GAPPS_PREFIX) || (SignatureUtils.canTrustSignature(pm) && pm.checkSignatures(packageName, GMS) == PackageManager.SIGNATURE_MATCH);
    }

    public static void increaseGmsCount(Context context, String packageName) {
        if (!GMS.equals(packageName) && isGapps(context.getPackageManager(), packageName)) {
            int gmsCount = GMS_COUNTER.incrementAndGet();
            PreventLog.v("increase gms reference: " + gmsCount + ", packageName: " + packageName);
        }
    }

    public static int getGmsCount() {
        return GMS_COUNTER.get();
    }

    public static int decreaseGmsCount(Context context, String packageName) {
        if (!GMS.equals(packageName) && isGapps(context.getPackageManager(), packageName)) {
            int gmsCount = GMS_COUNTER.decrementAndGet();
            PreventLog.v("decrease reference: " + gmsCount + ", packageName: " + packageName);
            return gmsCount;
        } else {
            return GMS_COUNTER.get();
        }
    }

    public static boolean isGcmAction(String sender, String action) {
        return GMS.equals(sender) && GCM_ACTIONS.contains(action);
    }

}
