package me.piebridge.prevent.ui.util;

import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;

/**
 * Created by thom on 15/7/23.
 */
public class PackageUtils {

    private PackageUtils() {

    }

    public static boolean isSystemPackage(int flags) {
        return (flags & (ApplicationInfo.FLAG_SYSTEM | ApplicationInfo.FLAG_UPDATED_SYSTEM_APP)) != 0;
    }

    private static boolean isSystemPackageWithoutLauncher(PackageManager pm, ApplicationInfo appInfo) {
        return isSystemPackage(appInfo.flags) && pm.getLaunchIntentForPackage(appInfo.packageName) == null;
    }

    public static boolean canPrevent(PackageManager pm, ApplicationInfo appInfo) {
        return pm != null && appInfo != null && appInfo.enabled && !isSystemPackageWithoutLauncher(pm, appInfo);
    }


}
