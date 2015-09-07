package me.piebridge.prevent.common;

import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.net.Uri;

import java.util.HashSet;
import java.util.Set;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.framework.SystemHook;

/**
 * Created by thom on 15/7/23.
 */
public class PackageUtils {

    private static Set<String> launchers;

    private PackageUtils() {

    }

    public static boolean isSystemPackage(int flags) {
        return (flags & (ApplicationInfo.FLAG_SYSTEM | ApplicationInfo.FLAG_UPDATED_SYSTEM_APP)) != 0;
    }

    public static boolean isSystemSignaturePackage(PackageManager pm, String packageName) {
        return pm.checkSignatures("android", packageName) != PackageManager.SIGNATURE_NO_MATCH;
    }

    private static synchronized void initLauncher(PackageManager pm) {
        if (launchers == null) {
            launchers = new HashSet<String>();
            Intent intent = new Intent(Intent.ACTION_MAIN);
            intent.addCategory(Intent.CATEGORY_HOME);
            for (ResolveInfo resolveInfo : pm.queryIntentActivities(intent, 0)) {
                launchers.add(resolveInfo.activityInfo.packageName);
            }
        }
    }

    private static boolean isLauncher(PackageManager pm, String packageName) {
        if (launchers == null) {
            initLauncher(pm);
        }
        return launchers.contains(packageName);
    }

    public static boolean canPrevent(PackageManager pm, ApplicationInfo appInfo) {
        if (appInfo.uid < SystemHook.FIRST_APPLICATION_UID) {
            return false;
        }
        // can prevent non-system package
        if (!isSystemPackage(appInfo.flags)) {
            return true;
        }
        // cannot prevent launcher
        if (isLauncher(pm, appInfo.packageName)) {
            return false;
        }
        // can prevent system packages with launcher
        if (pm.getLaunchIntentForPackage(appInfo.packageName) != null) {
            return true;
        }
        if (!isSystemSignaturePackage(pm, BuildConfig.APPLICATION_ID)) {
            return !isSystemSignaturePackage(pm, appInfo.packageName);
        } else {
            return GmsUtils.isGapps(pm, appInfo.packageName);
        }
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
