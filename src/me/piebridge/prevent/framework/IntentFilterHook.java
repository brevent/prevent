package me.piebridge.prevent.framework;

import android.accounts.AccountManager;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageParser;
import android.net.Uri;
import android.os.Binder;
import android.os.Process;

import java.util.Map;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.common.GmsUtils;
import me.piebridge.prevent.framework.util.AlarmManagerServiceUtils;
import me.piebridge.prevent.framework.util.BroadcastFilterUtils;
import me.piebridge.prevent.framework.util.LogUtils;
import me.piebridge.prevent.framework.util.NotificationManagerServiceUtils;
import me.piebridge.prevent.framework.util.SafeActionUtils;

/**
 * Created by thom on 15/8/11.
 */
public class IntentFilterHook {

    private static Context mContext;
    private static Map<String, Boolean> mPreventPackages;

    private IntentFilterHook() {

    }

    public static void setContext(Context context, Map<String, Boolean> preventPackages) {
        mPreventPackages = preventPackages;
        mContext = context;
    }

    public static boolean canHook(int result) {
        return result > 0 && SystemHook.isSystemHook() && mContext != null;
    }

    public static IntentFilterMatchResult hookBroadcastFilter(Object filter, Object[] args) {
        String action = (String) args[0];
        if (NotificationManagerServiceUtils.canHook(filter, action)) {
            return NotificationManagerServiceUtils.hook((Uri) args[0x3], mPreventPackages);
        } else if (AlarmManagerServiceUtils.canHook(args)) {
            return AlarmManagerServiceUtils.hook(filter);
        } else if (BuildConfig.DEBUG && Intent.ACTION_CLOSE_SYSTEM_DIALOGS.equals(action)) {
            return hookCloseSystemDialogs(filter, action);
        }
        return IntentFilterMatchResult.NONE;
    }

    private static IntentFilterMatchResult hookCloseSystemDialogs(Object filter, String action) {
        String packageName = BroadcastFilterUtils.getPackageName(filter);
        if (packageName != null && mPreventPackages.containsKey(packageName)) {
            LogUtils.logIntentFilter(true, "(ignore)", filter, action, packageName);
            return IntentFilterMatchResult.NO_MATCH;
        }
        return IntentFilterMatchResult.NONE;
    }

    private static IntentFilterMatchResult allowSafeIntent(PackageParser.ActivityIntentInfo filter, String sender, String action, String packageName) {
        LogUtils.logIntentFilterInfo(false, sender, filter, action, packageName);
        if (Boolean.TRUE.equals(mPreventPackages.get(packageName))) {
            PreventLog.i("allow " + packageName + " for next service/broadcast");
            mPreventPackages.put(packageName, false);
            SystemHook.restoreLater(packageName);
        }
        return IntentFilterMatchResult.NONE;
    }

    private static boolean isSystemSender(String sender) {
        return (sender == null || "android".equals(sender)) && Binder.getCallingUid() == Process.SYSTEM_UID;
    }

    private static boolean cannotPrevent(String packageName, String sender) {
        if (!Boolean.TRUE.equals(mPreventPackages.get(packageName))) {
            return true;
        } else if (packageName.equals(sender)) {
            return true;
        } else if (SystemHook.isSystemPackage(packageName) && SystemHook.hasRunningActivity(sender)) {
            return true;
        }
        return false;
    }

    private static boolean cannotPreventGms(String packageName, String sender) {
        return GmsUtils.isGms(packageName) && (GmsUtils.isGapps(mContext.getPackageManager(), sender) || GmsUtils.isGappsCaller(mContext));
    }

    private static boolean isSafeReceiverAction(boolean isSystem, String action) {
        return isSystem && !SafeActionUtils.isProtectedBroadcast(action);
    }

    private static boolean isSafeServiceAction(String action) {
        return "android.content.SyncAdapter".equals(action)
                || AccountManager.ACTION_AUTHENTICATOR_INTENT.equals(action)
                || GmsUtils.isGcmRegisterAction(action);
    }

    public static IntentFilterMatchResult hookActivityIntentInfo(PackageParser.ActivityIntentInfo filter, String sender, String action) {
        // for receiver, we don't block for activity
        PackageParser.Activity activity = filter.activity;
        PackageParser.Package owner = activity.owner;
        if (owner.receivers.contains(activity)) {
            return hookReceiver(filter, owner.applicationInfo, sender, action);
        } else {
            // we only care about receiver
            return IntentFilterMatchResult.NONE;
        }
    }

    private static IntentFilterMatchResult hookReceiver(PackageParser.ActivityIntentInfo filter, ApplicationInfo ai, String sender, String action) {
        String packageName = ai.packageName;
        if (cannotPrevent(packageName, sender)) {
            LogUtils.logIntentFilter(false, sender, filter, action, packageName);
            return IntentFilterMatchResult.NONE;
        }
        boolean isSystem = isSystemSender(sender);
        if (cannotPreventGms(packageName, sender)) {
            LogUtils.logIntentFilter(false, sender, filter, action, packageName);
            return IntentFilterMatchResult.NONE;
        } else if (GmsUtils.isGcmAction(sender, isSystem, action)) {
            return allowSafeIntent(filter, sender, action, packageName);
        } else if (isSafeReceiverAction(isSystem, action)) {
            LogUtils.logIntentFilter(false, sender, filter, action, packageName);
            return IntentFilterMatchResult.NONE;
        }
        // the default action is block, so change the log level
        LogUtils.logIntentFilter(true, sender, filter, action, packageName);
        return IntentFilterMatchResult.NO_MATCH;
    }

    public static IntentFilterMatchResult hookServiceIntentInfo(PackageParser.ServiceIntentInfo filter, String sender, String action) {
        PackageParser.Service service = filter.service;
        PackageParser.Package owner = service.owner;
        ApplicationInfo ai = owner.applicationInfo;
        String packageName = ai.packageName;
        if (cannotPrevent(packageName, sender)) {
            LogUtils.logIntentFilter(false, sender, filter, action, packageName);
            return IntentFilterMatchResult.NONE;
        }
        if (isSafeServiceAction(action)) {
            LogUtils.logIntentFilter(false, sender, filter, action, ai.packageName);
            return IntentFilterMatchResult.NONE;
        } else if (cannotPreventGms(packageName, sender)) {
            LogUtils.logIntentFilter(false, sender, filter, action, ai.packageName);
            return IntentFilterMatchResult.NONE;
        } else if (!isSystemSender(sender)) {
            LogUtils.logIntentFilterInfo(true, sender, filter, action, ai.packageName);
            return IntentFilterMatchResult.NO_MATCH;
        }
        LogUtils.logIntentFilter(false, sender, filter, action, packageName);
        return IntentFilterMatchResult.NONE;
    }

    public static boolean isPrevent(Intent intent) {
        String action = intent.getAction();
        return !SafeActionUtils.isProtectedBroadcast(action) || GmsUtils.isGcmAction(null, true, action);
    }


}
