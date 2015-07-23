package me.piebridge.util;

import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;

/**
 * Created by thom on 15/7/23.
 */
public class PackageUtils {

    private PackageUtils() {

    }

    private static boolean isSystemPackage(PackageManager pm, String packageName) {
        try {
            ApplicationInfo ai = pm.getApplicationInfo(packageName, 0);
            return isSystemPackage(ai.flags);
        } catch (PackageManager.NameNotFoundException e) {
            return false;
        }
    }

    public static boolean isSystemPackage(int flags) {
        return (flags & (ApplicationInfo.FLAG_SYSTEM | ApplicationInfo.FLAG_UPDATED_SYSTEM_APP)) != 0;
    }

    public static boolean isSystemPackage(Context context, String packageName) {
        return isSystemPackage(context.getPackageManager(), packageName);
    }

}
