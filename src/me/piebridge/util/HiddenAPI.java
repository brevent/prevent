package me.piebridge.util;

import android.app.ActivityManager;
import android.os.Process;

/**
 * Created by thom on 15/7/12.
 */
public class HiddenAPI {

    private HiddenAPI() {

    }

    public static int getUidForPid(int pid) {
        return Process.getUidForPid(pid);
    }

    public static int getParentPid(int pid) {
        return Process.getParentPid(pid);
    }

    public static void forceStopPackage(ActivityManager activityManager, String packageName) {
        activityManager.forceStopPackage(packageName);
    }

}