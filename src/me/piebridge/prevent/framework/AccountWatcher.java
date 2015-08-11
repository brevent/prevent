package me.piebridge.prevent.framework;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.accounts.AuthenticatorDescription;
import android.accounts.OnAccountsUpdateListener;
import android.content.Context;
import android.os.Handler;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Created by thom on 15/8/7.
 */
public class AccountWatcher implements OnAccountsUpdateListener {

    private static Map<String, String> types = new HashMap<String, String>();

    private static Set<String> mEnabledPackages = new HashSet<String>();

    private static AccountWatcher accountWatcher;

    private final AccountManager accountManager;

    private AccountWatcher(Context context, Handler handler) {
        accountManager = AccountManager.get(context);
        updateAuthDescriptions();
        accountManager.addOnAccountsUpdatedListener(this, handler, true);
    }

    public static AccountWatcher get(Context context, Handler handler) {
        accountWatcher = new AccountWatcher(context, handler);
        return accountWatcher;
    }

    public static void onPackageAdded() {
        if (accountWatcher != null) {
            accountWatcher.updateAuthDescriptions();
        }
    }

    private void updateAuthDescriptions() {
        for (AuthenticatorDescription ad : accountManager.getAuthenticatorTypes()) {
            PreventLog.v("updateAuthDescriptions, type: " + ad.type + ", packageName: " + ad.packageName);
            if (ad.packageName != null) {
                types.put(ad.type, ad.packageName);
            }
        }
    }

    @Override
    public void onAccountsUpdated(Account[] accounts) {
        Account[] updatedAccounts;
        if (accounts == null) {
            updatedAccounts = accountManager.getAccounts();
        } else {
            updatedAccounts = accounts;
        }
        mEnabledPackages.clear();
        for (Account account : updatedAccounts) {
            String packageName = types.get(account.type);
            PreventLog.v("onAccountUpdated, type: " + account.type + ", packageName: " + packageName);
            if (packageName != null && !mEnabledPackages.contains(packageName)) {
                mEnabledPackages.add(packageName);
            }
        }
        PreventLog.d("enabled accounts: " + mEnabledPackages);
    }

    public boolean containsPackage(String packageName) {
        return mEnabledPackages.contains(packageName);
    }

}
