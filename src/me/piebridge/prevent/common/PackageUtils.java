package me.piebridge.prevent.common;

import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.net.Uri;

/**
 * Created by thom on 15/7/23.
 */
public class PackageUtils {

    private PackageUtils() {

    }

    public static boolean isSystemPackage(int flags) {
        return (flags & (ApplicationInfo.FLAG_SYSTEM | ApplicationInfo.FLAG_UPDATED_SYSTEM_APP)) != 0;
    }

    public static boolean isSystemPackageWithoutLauncher(PackageManager pm, ApplicationInfo appInfo) {
        return isSystemPackage(appInfo.flags) && pm.getLaunchIntentForPackage(appInfo.packageName) == null;
    }

    public static boolean canPrevent(PackageManager pm, ApplicationInfo appInfo) {
        return !isSystemPackageWithoutLauncher(pm, appInfo) || GmsUtils.isGapps(pm, appInfo.packageName);
    }

    public static String getPackageName(Intent intent) {
        Uri data = intent.getData();
        if (data != null) {
            return data.getSchemeSpecificPart();
        } else {
            return null;
        }
    }

}
