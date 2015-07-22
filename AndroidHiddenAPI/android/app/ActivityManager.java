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
    }

    public static class RunningServiceInfo {
    }
}
