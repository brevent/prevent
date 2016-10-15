package me.piebridge.prevent.common;

import android.app.ActivityManager;
import android.os.SystemClock;

import java.util.LinkedHashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * Created by thom on 2016/10/20.
 */
public class TimeUtils {

    private static long MAX_IMPORTANCE = 0;
    private static final Set<Long> IMPORTANCES = new LinkedHashSet<Long>();

    static {
        IMPORTANCES.add((long) ActivityManager.RunningAppProcessInfo.IMPORTANCE_BACKGROUND);
        IMPORTANCES.add((long) ActivityManager.RunningAppProcessInfo.IMPORTANCE_EMPTY);
        IMPORTANCES.add((long) ActivityManager.RunningAppProcessInfo.IMPORTANCE_FOREGROUND);
        IMPORTANCES.add((long) ActivityManager.RunningAppProcessInfo.IMPORTANCE_FOREGROUND_SERVICE);
        IMPORTANCES.add((long) ActivityManager.RunningAppProcessInfo.IMPORTANCE_GONE);
        IMPORTANCES.add((long) ActivityManager.RunningAppProcessInfo.IMPORTANCE_PERCEPTIBLE);
        IMPORTANCES.add((long) ActivityManager.RunningAppProcessInfo.IMPORTANCE_SERVICE);
        IMPORTANCES.add((long) ActivityManager.RunningAppProcessInfo.IMPORTANCE_TOP_SLEEPING);
        IMPORTANCES.add((long) ActivityManager.RunningAppProcessInfo.IMPORTANCE_VISIBLE);
        for (Long importance : IMPORTANCES) {
            if (importance > MAX_IMPORTANCE) {
                MAX_IMPORTANCE = importance;
            }
        }
    }

    private TimeUtils() {

    }

    public static long now() {
        return TimeUnit.MILLISECONDS.toSeconds(SystemClock.elapsedRealtime());
    }

    public static long fixImportance(long time) {
        if (time > MAX_IMPORTANCE) {
            return time;
        }
        long fixed = time;
        while (IMPORTANCES.contains(fixed)) {
            fixed += 1;
        }
        return fixed;
    }

}
