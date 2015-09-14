package me.piebridge.prevent.framework.util;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.accounts.AuthenticatorDescription;
import android.content.ContentResolver;
import android.content.Context;
import android.content.SyncAdapterType;
import android.os.Build;

import java.util.HashMap;
import java.util.Map;

import me.piebridge.prevent.framework.PreventLog;

/**
 * Created by thom on 15/8/7.
 */
public class AccountUtils {

    private static Map<String, String> packageTypes = new HashMap<String, String>();

    private AccountUtils() {

    }

    public static void updateAuthDescriptions(Context context) {
        packageTypes.clear();
        AccountManager accountManager = AccountManager.get(context);
        for (AuthenticatorDescription ad : accountManager.getAuthenticatorTypes()) {
            if (ad.packageName != null) {
                PreventLog.d("ad type: " + ad.type + ", packageName: " + ad.packageName);
                packageTypes.put(ad.packageName, ad.type);
            }
        }
    }

    public static boolean isPackageSyncable(Context context, String packageName) {
        if (!ContentResolver.getMasterSyncAutomatically()) {
            return false;
        }
        if (packageTypes.isEmpty()) {
            updateAuthDescriptions(context);
        }
        String accountType = packageTypes.get(packageName);
        if (accountType == null) {
            return false;
        }
        PreventLog.d("check sync for " + packageName + ", account type: " + accountType);
        for (SyncAdapterType type : ContentResolver.getSyncAdapterTypes()) {
            if (accountType.equals(type.accountType) && isSyncable(context, type)) {
                PreventLog.i("package " + packageName + " is syncable");
                return true;
            }
        }
        return false;
    }

    private static boolean isSyncable(Context context, SyncAdapterType type) {
        // skip always syncable
        if (isAlwaysSyncable(type)) {
            PreventLog.d("type " + type.accountType + ", authority" + type.authority + " is always syncable");
            return false;
        }
        AccountManager accountManager = AccountManager.get(context);
        Account[] accounts = accountManager.getAccountsByType(type.accountType);
        for (Account account : accounts) {
            if (ContentResolver.getSyncAutomatically(account, type.authority)) {
                PreventLog.d("type " + type.accountType + ", authority" + type.authority + " is syncable");
                return true;
            }
        }
        return false;
    }

    private static boolean isAlwaysSyncable(SyncAdapterType type) {
        return Build.VERSION.SDK_INT > Build.VERSION_CODES.GINGERBREAD_MR1 && type.isAlwaysSyncable();
    }

    public static boolean cannotHook(Context context, String action, String packageName) {
        return AccountManager.ACTION_AUTHENTICATOR_INTENT.equals(action) && isPackageSyncable(context, packageName);
    }

    public static void onPackageChanged(String packageName) {
        if (packageName != null) {
            packageTypes.clear();
        }
    }

}
