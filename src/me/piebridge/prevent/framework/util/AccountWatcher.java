package me.piebridge.prevent.framework.util;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.accounts.OnAccountsUpdateListener;
import android.content.ComponentName;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.SyncAdapterType;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.content.pm.ServiceInfo;
import android.content.res.Resources;
import android.content.res.XmlResourceParser;
import android.os.Handler;
import android.os.HandlerThread;
import android.support.annotation.Nullable;
import android.text.TextUtils;

import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import me.piebridge.prevent.framework.PreventLog;

/**
 * Created by thom on 15/8/7.
 */
public class AccountWatcher implements OnAccountsUpdateListener {

    private static final String NAMESPACE_ANDROID = "http://schemas.android.com/apk/res/android";
    private static final String SYNC_ADAPTER = "android.content.SyncAdapter";
    private final Context mContext;
    private Collection<Account> mEnabledAccounts = null;

    public AccountWatcher(Context context) {
        mContext = context;
        AccountManager.get(context).addOnAccountsUpdatedListener(this, initHandler(), false);
    }

    private Handler initHandler() {
        HandlerThread thread = new HandlerThread("AccountWatcher");
        thread.start();
        return new Handler(thread.getLooper());
    }

    private static SyncAdapterType getSyncAdapter(Context context, ComponentName cn) {
        XmlResourceParser parser = null;
        try {
            PackageManager pm = context.getPackageManager();
            ServiceInfo si = pm.getServiceInfo(cn, PackageManager.GET_META_DATA);
            Resources resources = pm.getResourcesForApplication(si.packageName);
            parser = si.loadXmlMetaData(pm, SYNC_ADAPTER);
            while (true) {
                int type = parser.next();
                if (type == XmlPullParser.START_TAG && "sync-adapter".equals(parser.getName())) {
                    String contentAuthority = getValue(resources, parser.getAttributeValue(NAMESPACE_ANDROID, "contentAuthority"), "");
                    String accountType = getValue(resources, parser.getAttributeValue(NAMESPACE_ANDROID, "accountType"), "");
                    boolean userVisible = Boolean.valueOf(getValue(resources, parser.getAttributeValue(NAMESPACE_ANDROID, "userVisible"), "true"));
                    boolean supportsUploading = Boolean.valueOf(getValue(resources, parser.getAttributeValue(NAMESPACE_ANDROID, "supportsUploading"), "true"));
                    PreventLog.v(cn.flattenToShortString() + ", accountType: " + accountType + ", contentAuthority: " + contentAuthority
                            + ", userVisible: " + userVisible + ", supportsUploading: " + supportsUploading);
                    return new SyncAdapterType(contentAuthority, accountType, userVisible, supportsUploading);
                } else if (type == XmlPullParser.END_DOCUMENT) {
                    break;
                }
            }
        } catch (XmlPullParserException e) {
            PreventLog.d("cannot parse " + cn.flattenToShortString(), e);
        } catch (IOException e) {
            PreventLog.d("cannot parse/io " + cn.flattenToShortString(), e);
        } catch (PackageManager.NameNotFoundException e) {
            PreventLog.d("cannot find " + cn.flattenToShortString(), e);
        } finally {
            if (parser != null) {
                parser.close();
            }
        }
        return null;
    }

    private static String getValue(Resources resources, String value, String defaultValue) {
        if (value != null && value.startsWith("@")) {
            String number = value.substring(1);
            if (TextUtils.isDigitsOnly(number)) {
                try {
                    return resources.getString(Integer.valueOf(number));
                } catch (Resources.NotFoundException e) {
                    PreventLog.d("cannot find " + value, e);
                }
            }
        }
        if (value == null) {
            return defaultValue;
        } else {
            return value;
        }
    }

    public boolean isComponentSyncable(ComponentName component) {
        PreventLog.d("check sync for " + component.flattenToShortString());
        SyncAdapterType type = getSyncAdapter(mContext, component);
        if (type == null) {
            PreventLog.w("cannot find sync adapter for " + component.flattenToShortString());
            return false;
        } else if (type.isUserVisible() && isSyncable(type)) {
            PreventLog.d(component.flattenToShortString() + " is syncable, account type: " + type.accountType + ", authority: " + type.authority);
            return true;
        } else {
            PreventLog.d(component.flattenToShortString() + " isn't syncable");
            return false;
        }
    }

    @Nullable
    public Boolean isPackageSyncable(String packageName) {
        PackageManager pm = mContext.getPackageManager();
        Intent intent = new Intent(SYNC_ADAPTER);
        intent.setPackage(packageName);
        Boolean result = null;
        for (ResolveInfo info : pm.queryIntentServices(intent, 0)) {
            ServiceInfo si = info.serviceInfo;
            if (packageName.equals(si.packageName)) {
                result = false;
                if (isComponentSyncable(new ComponentName(si.packageName, si.name))) {
                    return true;
                }
            }
        }
        return result;
    }

    public void setSyncable(String packageName, boolean syncable) {
        PackageManager pm = mContext.getPackageManager();
        Intent intent = new Intent(SYNC_ADAPTER);
        intent.setPackage(packageName);
        for (ResolveInfo info : pm.queryIntentServices(intent, 0)) {
            ServiceInfo si = info.serviceInfo;
            if (packageName.equals(si.packageName)) {
                setSyncable(new ComponentName(si.packageName, si.name), syncable);
            }
        }
    }

    private void setSyncable(ComponentName component, boolean syncable) {
        PreventLog.d("set sync to " + syncable + " for " + component.flattenToShortString());
        SyncAdapterType type = getSyncAdapter(mContext, component);
        if (type != null && type.isUserVisible()) {
            for (Account account : getEnabledAccounts()) {
                if (account.type.equals(type.accountType)) {
                    ContentResolver.setSyncAutomatically(account, type.authority, syncable);
                }
            }
        }
    }

    private boolean isSyncable(SyncAdapterType type) {
        for (Account account : getEnabledAccounts()) {
            if (account.type.equals(type.accountType)) {
                PreventLog.v("check account " + account.type + " for " + type.authority);
                if (ContentResolver.getSyncAutomatically(account, type.authority)
                        && ContentResolver.getIsSyncable(account, type.authority) > 0) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public void onAccountsUpdated(Account[] accounts) {
        Collection<Account> enabledAccounts = getEnabledAccounts();
        enabledAccounts.clear();
        Collections.addAll(enabledAccounts, accounts);
        PreventLog.i("accounts: " + enabledAccounts.size());
    }

    public Collection<Account> getEnabledAccounts() {
        if (mEnabledAccounts == null) {
            mEnabledAccounts = new ArrayList<Account>();
            try {
                Collections.addAll(mEnabledAccounts, AccountManager.get(mContext).getAccounts());
            } catch (RuntimeException e) {
                PreventLog.e("cannot find system's account", e);
            }
        }
        return mEnabledAccounts;
    }

}
