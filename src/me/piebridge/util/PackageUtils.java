package me.piebridge.util;

import android.content.ContentValues;
import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.res.Resources;

import me.piebridge.forcestopgb.ui.Provider;

/**
 * Created by thom on 15/7/23.
 */
public class PackageUtils {

    private PackageUtils() {

    }

    public static boolean cannotPrevents(Context context, String packageName) {
        PackageManager pm = context.getPackageManager();
        return isSystemPackage(pm, packageName) && hasNoLauncher(pm, packageName);
    }

    private static boolean hasNoLauncher(PackageManager pm, String packageName) {
        return pm.getLaunchIntentForPackage(packageName) == null;
    }

    private static boolean isSystemPackage(PackageManager pm, String packageName) {
        try {
            ApplicationInfo ai = pm.getApplicationInfo(packageName, 0);
            return (ai.flags & (ApplicationInfo.FLAG_SYSTEM | ApplicationInfo.FLAG_UPDATED_SYSTEM_APP)) != 0;
        } catch (PackageManager.NameNotFoundException e) {
            return false;
        }
    }

    public static boolean isSystemPackage(Context context, String packageName) {
        return isSystemPackage(context.getPackageManager(), packageName);
    }
}
