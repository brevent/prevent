package me.piebridge.prevent.framework;

import android.appwidget.AppWidgetManager;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageParser;
import android.net.Uri;

import java.util.Set;

import me.piebridge.PreventRunningHook;
import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.framework.util.BroadcastFilterUtils;
import me.piebridge.prevent.framework.util.LogcatUtils;
import me.piebridge.prevent.framework.util.SafeActionUtils;

/**
 * Created by thom on 15/10/27.
 */
public class PreventRunning implements PreventRunningHook {

    private final ThreadLocal<String> mSender;

    public PreventRunning() {
        mSender = new ThreadLocal<String>();
        PreventLog.i("prevent running " + BuildConfig.VERSION_NAME);
        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        SystemHook.setClassLoader(classLoader);
        LogcatUtils.logcat("*:v");
    }

    @Override
    public void setSender(String sender) {
        mSender.set(sender);
    }

    @Override
    public void onBroadcastIntent(Intent intent) {
        String action = intent.getAction();
        if (AppWidgetManager.ACTION_APPWIDGET_ENABLED.equals(action)) {
            SafeActionUtils.updateWidget(intent.getComponent(), true);
        } else if (AppWidgetManager.ACTION_APPWIDGET_DISABLED.equals(action)) {
            SafeActionUtils.updateWidget(intent.getComponent(), false);
        }
    }

    @Override
    public void onCleanUpRemovedTask(String packageName) {
        ActivityManagerServiceHook.onCleanUpRemovedTask(packageName);
    }

    @Override
    public void onStartHomeActivity(String packageName) {
        SystemHook.onStartHomeActivity(packageName);
    }

    @Override
    public void onMoveActivityTaskToBack(String packageName) {
        SystemHook.onMoveActivityToBack(packageName);
    }

    @Override
    public void onAppDied(Object processRecord) {
        SystemHook.onAppDied(processRecord);
    }

    @Override
    public void onLaunchActivity(Object activityRecord) {
        SystemHook.onLaunchActivity(activityRecord);
    }

    @Override
    public void onResumeActivity(Object activityRecord) {
        SystemHook.onResumeActivity(activityRecord);
    }

    @Override
    public void onUserLeavingActivity(Object activityRecord) {
        SystemHook.onUserLeavingActivity(activityRecord);
    }

    @Override
    public void onDestroyActivity(Object activityRecord) {
        SystemHook.onDestroyActivity(activityRecord);
    }

    @Override
    public boolean isExcludingStopped(String action) {
        return !SafeActionUtils.isSafeAction(action);
    }

    @Override
    public boolean hookStartProcessLocked(Context context, ApplicationInfo info, String hostingType, ComponentName hostingName) {
        return ActivityManagerServiceHook.hookStartProcessLocked(context, info, hostingType, hostingName, mSender.get());
    }

    @Override
    public int match(int match, Object filter, String action, String type, String scheme, Uri data, Set<String> categories) {
        if (IntentFilterHook.canHook(match)) {
            IntentFilterMatchResult result;
            if (filter instanceof PackageParser.ActivityIntentInfo) {
                result = IntentFilterHook.hookActivityIntentInfo((PackageParser.ActivityIntentInfo) filter, mSender.get(), action);
            } else if (filter instanceof PackageParser.ServiceIntentInfo) {
                result = IntentFilterHook.hookServiceIntentInfo((PackageParser.ServiceIntentInfo) filter, mSender.get(), action);
            } else if (BroadcastFilterUtils.isBroadcastFilter(filter)) {
                result = IntentFilterHook.hookBroadcastFilter(filter, action, data, categories);
            } else {
                result = IntentFilterMatchResult.NONE;
            }
            if (!result.isNone()) {
                return result.getResult();
            }
        }
        return match;
    }

    public void setVersion(int version) {
        PreventLog.d("bridge version: " + version);
        SystemHook.setVersion(version);
    }

    public void setMethod(String method) {
        PreventLog.d("bridge method: " + method);
        SystemHook.setMethod(method);
    }
}