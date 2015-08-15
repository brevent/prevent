package me.piebridge.prevent.common;

import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.*;
import android.os.Process;

import java.util.concurrent.atomic.AtomicInteger;

import me.piebridge.prevent.framework.PreventLog;
import me.piebridge.prevent.framework.SystemHook;

/**
 * Created by thom on 15/7/28.
 */
public class GmsUtils {

    private static int gmsUid;
    private static AtomicInteger gmsCounter = new AtomicInteger();
    public static final String GMS = "com.google.android.gms";
    public static final String GAPPS_PREFIX = "com.google.android.";
    public static final String ACTION_RECEIVE = "com.google.android.c2dm.intent.RECEIVE";

    private GmsUtils() {

    }

    public static boolean isGapps(PackageManager pm, String packageName) {
        return pm.checkSignatures(packageName, GMS) == PackageManager.SIGNATURE_MATCH ||
                (packageName.startsWith(GAPPS_PREFIX) && pm.getLaunchIntentForPackage(GAPPS_PREFIX) != null);
    }

    public static void increaseGmsCount(Context context, String packageName) {
        if (!GMS.equals(packageName) && isGapps(context.getPackageManager(), packageName)) {
            int gmsCount = gmsCounter.incrementAndGet();
            PreventLog.v("increase gms reference: " + gmsCount + ", packageName: " + packageName);
        }
    }

    public static int decreaseGmsCount(Context context, String packageName) {
        if (!GMS.equals(packageName) && isGapps(context.getPackageManager(), packageName)) {
            int gmsCount = gmsCounter.decrementAndGet();
            PreventLog.v("decrease reference: " + gmsCount + ", packageName: " + packageName);
            return gmsCount;
        } else {
            return gmsCounter.get();
        }
    }

    public static boolean isGcmAction(Context context, String action) {
        if (GmsUtils.ACTION_RECEIVE.equals(action)) {
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
