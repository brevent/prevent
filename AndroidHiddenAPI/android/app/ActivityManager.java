package android.app;

import android.content.ComponentName;

import java.util.List;

public class ActivityManager {

    /**
     * @hide
     */
    public void forceStopPackage(String packageName) {
        throw new UnsupportedOperationException();
    }

    public List<RunningServiceInfo> getRunningServices(int maxNum) throws SecurityException {
        throw new UnsupportedOperationException();
    }

    public List<RunningAppProcessInfo> getRunningAppProcesses() {
        throw new UnsupportedOperationException();
    }

    public static class RunningAppProcessInfo {
        public static final int IMPORTANCE_BACKGROUND = 400;
        public static final int IMPORTANCE_EMPTY = 500;
        public static final int IMPORTANCE_FOREGROUND = 100;
        public static final int IMPORTANCE_FOREGROUND_SERVICE = 125;
        public static final int IMPORTANCE_GONE = 1000;
        public static final int IMPORTANCE_PERCEPTIBLE = 130;
        public static final int IMPORTANCE_SERVICE = 300;
        public static final int IMPORTANCE_TOP_SLEEPING = 150;
        public static final int IMPORTANCE_VISIBLE = 200;
        public int importance;
        public int pid;
        public String[] pkgList = null;
        public int uid;
    }

    public static class RunningServiceInfo {
        public int clientCount;
        public int clientLabel;
        public int flags;
        public int pid;
        public String process;
        public ComponentName service;
        public boolean started;
        public int uid;
    }

}
