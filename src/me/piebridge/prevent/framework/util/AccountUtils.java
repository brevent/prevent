package me.piebridge.prevent.framework.util;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.content.ContentResolver;
import android.content.Context;
import android.content.SyncAdapterType;
import android.content.pm.ProviderInfo;

import me.piebridge.prevent.framework.PreventLog;

/**
 * Created by thom on 15/8/7.
 */
public class AccountUtils {

    private static final String PACKAGE = "package ";

    private AccountUtils() {

    }

    public static boolean isPackageSyncable(Context context, String packageName) {
        if (!ContentResolver.getMasterSyncAutomatically()) {
            return false;
        }
        PreventLog.d("check sync for " + packageName);
        for (SyncAdapterType type : ContentResolver.getSyncAdapterTypes()) {
            if (!type.isUserVisible()) {
                continue;
            }
            ProviderInfo pi = context.getPackageManager().resolveContentProvider(type.authority, 0);
            if (pi != null && packageName.equals(pi.packageName) && isSyncable(context, type)) {
                PreventLog.d(PACKAGE + packageName + " is syncable");
                return true;
            }
        }
        PreventLog.d(PACKAGE + packageName + " isn't syncable");
        return false;
    }

    private static boolean isSyncable(Context context, SyncAdapterType type) {
        AccountManager accountManager = AccountManager.get(context);
        Account[] accounts = accountManager.getAccountsByType(type.accountType);
        for (Account account : accounts) {
            if (isSyncable(account, type.authority)) {
                return true;
            }
        }
        return false;
    }

    private static boolean isSyncable(Account account, String authority) {
        PreventLog.d("check sync for account type: " + account.type + ", authority: " + authority);
        return ContentResolver.getSyncAutomatically(account, authority)
                && (ContentResolver.getIsSyncable(account, authority) > 0);
    }

}
