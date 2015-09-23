package me.piebridge.prevent.framework.util;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.accounts.AuthenticatorDescription;
import android.content.ComponentName;
import android.content.ContentResolver;
import android.content.Context;
import android.content.SyncAdapterType;
import android.content.pm.ProviderInfo;

import me.piebridge.prevent.framework.PreventLog;

/**
 * Created by thom on 15/8/7.
 */
public class AccountUtils {

    private AccountUtils() {

    }

    public static boolean isComponentSyncable(Context context, ComponentName component) {
        if (!ContentResolver.getMasterSyncAutomatically()) {
            return false;
        }
        PreventLog.d("check sync for " + component.flattenToShortString());
        AccountManager accountManager = AccountManager.get(context);
        for (AuthenticatorDescription description : accountManager.getAuthenticatorTypes()) {
            if (isSyncable(context, description, component)) {
                PreventLog.d(component.flattenToShortString() + " is syncable");
                return true;
            }
        }
        PreventLog.d(component.flattenToShortString() + " isn't syncable");
        return false;
    }

    private static boolean isSyncable(Context context, AuthenticatorDescription description, ComponentName component) {
        String packageName = component.getPackageName();
        AccountManager accountManager = AccountManager.get(context);
        for (SyncAdapterType type : ContentResolver.getSyncAdapterTypes()) {
            ProviderInfo provider = context.getPackageManager().resolveContentProvider(type.authority, 0);
            if (provider == null || !type.isUserVisible() || !type.accountType.equals(description.type)) {
                continue;
            }
            if (isSamePackage(packageName, description.packageName, provider.packageName) && isSyncable(accountManager, type)) {
                PreventLog.d("syncable, account type: " + type.accountType + ", authority: " + type.authority
                        + ", authenticator package: " + description.packageName + ", provider package: " + provider.packageName);
                return true;
            }
        }
        return false;
    }

    private static boolean isSamePackage(String packageName, String descriptionPackageName, String providerPackageName) {
        if (packageName.equals(descriptionPackageName) || packageName.equals(providerPackageName)) {
            return true;
        } else if ("com.android.providers.calendar".equals(providerPackageName) && "com.google.android.calendar".equals(packageName)) {
            // should use better way, however, i think hardcode is ok
            PreventLog.d("consider " + packageName + " equals " + providerPackageName);
            return true;
        } else {
            return false;
        }
    }

    private static boolean isSyncable(AccountManager accountManager, SyncAdapterType type) {
        Account[] accounts = accountManager.getAccountsByType(type.accountType);
        for (Account account : accounts) {
            if (ContentResolver.getSyncAutomatically(account, type.authority)
                    && ContentResolver.getIsSyncable(account, type.authority) > 0) {
                return true;
            }
        }
        return false;
    }

}
