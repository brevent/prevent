package me.piebridge.prevent.ui.util;

import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;

import me.piebridge.prevent.ui.UILog;

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
            UILog.d("cannot find package " + e, e);
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
