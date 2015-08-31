package me.piebridge.prevent.common;

import android.content.Context;
import android.content.pm.PackageManager;
import android.os.Binder;

import java.util.Arrays;
import java.util.Collection;
import java.util.concurrent.atomic.AtomicInteger;

import me.piebridge.prevent.framework.PreventLog;
import me.piebridge.prevent.framework.SystemHook;

/**
 * Created by thom on 15/7/28.
 */
public class GmsUtils {

    private static int gmsUid;
    public static final String GMS = "com.google.android.gms";
    public static final String GAPPS_PREFIX = "com.google.android.";
    private static final AtomicInteger GMS_COUNTER = new AtomicInteger();
    private static Collection<String> GCM_ACTIONS = Arrays.asList("com.google.android.c2dm.intent.RECEIVE",
            "com.google.android.c2dm.intent.REGISTRATION");

    private GmsUtils() {

    }

    public static boolean isGapps(PackageManager pm, String packageName) {
        return pm.checkSignatures(packageName, GMS) == PackageManager.SIGNATURE_MATCH ||
                packageName.startsWith(GAPPS_PREFIX);
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

    public static boolean isGcmAction(Context context, String action) {
        if (GCM_ACTIONS.contains(action)) {
            int callingUid = Binder.getCallingUid();
            if (callingUid == gmsUid || callingUid < SystemHook.FIRST_APPLICATION_UID) {
                return true;
            }
            String[] packages = context.getPackageManager().getPackagesForUid(callingUid);
            if (packages == null) {
                return false;
            }
            for (String packageName : packages) {
                PreventLog.d("user " + callingUid + " has package " + packageName);
                if (GMS.equals(packageName)) {
                    gmsUid = callingUid;
                    return true;
                }
            }
        }
        return false;
    }

}
