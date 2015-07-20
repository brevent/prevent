package android.app;

import java.util.List;

import android.content.ComponentName;

public class ActivityManager {

    /**
     * @hide
     */
    public void forceStopPackage(String packageName) {
    }

    public List<RunningServiceInfo> getRunningServices(int maxNum) throws SecurityException {
        return null;
    }

    public List<RunningAppProcessInfo> getRunningAppProcesses() {
        return null;
    }

    public static class RunningAppProcessInfo {
        public static final int IMPORTANCE_FOREGROUND = 100;
        public static final int IMPORTANCE_VISIBLE = 200;
        public static final int IMPORTANCE_PERCEPTIBLE = 130;
        public static final int IMPORTANCE_SERVICE = 300;
        public static final int IMPORTANCE_BACKGROUND = 400;
        public static final int IMPORTANCE_EMPTY = 500;
        public int importance;
        public String pkgList[];
    }

    public static class RunningServiceInfo {
        public ComponentName service;
    }
}
