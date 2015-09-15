package me.piebridge.prevent.common;

import android.content.Context;
import android.content.pm.PackageManager;
import android.os.Binder;

import java.util.Arrays;
import java.util.Collection;
import java.util.concurrent.atomic.AtomicInteger;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.framework.PreventLog;
import me.piebridge.prevent.framework.SystemHook;

/**
 * Created by thom on 15/7/28.
 */
public class GmsUtils {

    private static final String GMS = "com.google.android.gms";
    private static final String GSF = "com.google.android.gsf";
    private static final String GSF_LOGIN = "com.google.android.gsf.login";
    private static final String GAPPS_PREFIX = "com.google.android.";
    private static final AtomicInteger GMS_COUNTER = new AtomicInteger();
    // https://developers.google.com/cloud-messaging/android/client
    private static Collection<String> GCM_ACTIONS = Arrays.asList(
            "com.google.android.c2dm.intent.RECEIVE",
            "com.google.android.c2dm.intent.REGISTRATION");
    private static final String GCM_ACTION_REGISTER = "com.google.android.c2dm.intent.REGISTER";
    private static Collection<String> GMS_PACKAGES = Arrays.asList(
            GMS, GSF, GSF_LOGIN
    );
    private static Collection<String> GAPPS = Arrays.asList(
            "com.android.chrome", "com.android.facelock", "com.android.vending"
    );

    private GmsUtils() {

    }

    public static boolean isGapps(String packageName) {
        return packageName != null && (packageName.startsWith(GAPPS_PREFIX) || GAPPS.contains(packageName));
    }

    public static boolean isGapps(PackageManager pm, String packageName) {
        return isGapps(packageName) || (pm.checkSignatures(BuildConfig.APPLICATION_ID, GMS) != PackageManager.SIGNATURE_MATCH && pm.checkSignatures(packageName, GMS) == PackageManager.SIGNATURE_MATCH);
    }

    public static void increaseGmsCount(Context context, String packageName) {
        if (!GMS.equals(packageName) && isGapps(context.getPackageManager(), packageName)) {
            int gmsCount = GMS_COUNTER.incrementAndGet();
            PreventLog.i("increase gms reference: " + gmsCount + ", package: " + packageName);
        }
    }

    public static void decreaseGmsCount(Context context, String packageName) {
        if (!GMS.equals(packageName) && isGapps(context.getPackageManager(), packageName)) {
            int gmsCount = GMS_COUNTER.decrementAndGet();
            PreventLog.i("decrease gms reference: " + gmsCount + ", package: " + packageName);
        }
    }

    public static boolean isGcmAction(String sender, boolean isSystem, String action) {
        return (isSystem || isGms(sender)) && GCM_ACTIONS.contains(action);
    }

    public static boolean isGcmRegisterAction(String action) {
        return GCM_ACTION_REGISTER.equals(action);
    }

    public static boolean isGms(String packageName) {
        return GMS_PACKAGES.contains(packageName);
    }

    public static Collection<String> getGmsPackages() {
        return GMS_PACKAGES;
    }

    public static boolean isGappsCaller(Context context) {
        try {
            int callingUid = Binder.getCallingUid();
            if (callingUid < SystemHook.FIRST_APPLICATION_UID) {
                return false;
            }
            PackageManager pm = context.getPackageManager();
            return pm.getApplicationInfo(GMS, 0).uid == callingUid || isGapps(pm, callingUid);
        } catch (PackageManager.NameNotFoundException e) {
            PreventLog.v("cannot find gms", e);
        }
        return false;
    }

    private static boolean isGapps(PackageManager pm, int callingUid) {
        String[] packageNames = pm.getPackagesForUid(callingUid);
        if (packageNames == null) {
            return false;
        }
        for (String packageName : packageNames) {
            if (isGapps(packageName)) {
                return true;
            }
        }
        return false;
    }

    public static boolean canStopGms() {
        int gmsCount = GMS_COUNTER.get();
        if (gmsCount != 0) {
            PreventLog.i("cannot stop gms now, gms reference: " + gmsCount);
            return false;
        } else {
            // I think it's logged already
            return !SystemHook.hasRunningGapps();
        }
    }

}
