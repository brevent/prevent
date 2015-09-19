package me.piebridge.prevent.ui;

import android.app.ActivityManager.RunningAppProcessInfo;
import android.app.ProgressDialog;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.graphics.Bitmap;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.ListFragment;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.util.TypedValue;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView.AdapterContextMenuInfo;
import android.widget.ArrayAdapter;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.EditText;
import android.widget.Filter;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;

import java.text.Collator;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import me.piebridge.forcestopgb.R;
import me.piebridge.prevent.common.GmsUtils;
import me.piebridge.prevent.common.PackageUtils;
import me.piebridge.prevent.ui.util.PreventUtils;

public abstract class PreventFragment extends ListFragment {

    private Adapter mAdapter;
    private PreventActivity mActivity;
    private Set<String> prevNames = null;
    private View filter;
    private CheckBox check;
    private EditText search;
    private int headerIconWidth;
    private static final int HEADER_ICON_WIDTH = 48;
    private static Map<String, Position> positions = new HashMap<String, Position>();

    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);
        registerForContextMenu(getListView());
        mActivity = (PreventActivity) getActivity();
        if (mActivity != null) {
            setNewAdapterIfNeeded(mActivity, true);
        }
    }

    @Override
    public void onDestroyView() {
        saveListPosition();
        super.onDestroyView();
        mActivity = null;
        setListAdapter(null);
    }

    private void selectAll(boolean checked) {
        if (mActivity != null && mAdapter != null) {
            Set<String> selections = mActivity.getSelection();
            if (checked) {
                selections.addAll(mAdapter.getAllPackages());
            } else {
                selections.clear();
            }
            mAdapter.notifyDataSetChanged();
        }
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.list, container, false);
        filter = view.findViewById(R.id.filter);
        check = (CheckBox) filter.findViewById(R.id.filter_check);
        check.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                selectAll(check.isChecked());
            }
        });
        search = (EditText) filter.findViewById(R.id.filter_query);
        search.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int before, int after) {
                // do nothing
            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int after) {
                if (mAdapter != null) {
                    mAdapter.getFilter().filter(s);
                }
            }

            @Override
            public void afterTextChanged(Editable s) {
                // do nothing
            }
        });
        search.setHint(getQueryHint());
        return view;
    }

    @Override
    public void onListItemClick(ListView l, View v, int position, long id) {
        l.showContextMenuForChild(v);
    }

    @Override
    public void onPause() {
        saveListPosition();
        super.onPause();
    }

    private int getHeaderIconWidth() {
        if (headerIconWidth == 0) {
            headerIconWidth = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, HEADER_ICON_WIDTH, getResources().getDisplayMetrics());
        }
        return headerIconWidth;
    }

    @Override
    public void onCreateContextMenu(ContextMenu menu, View v, ContextMenuInfo menuInfo) {
        if (mActivity == null || menu == null || menuInfo == null) {
            return;
        }
        menu.clear();
        ViewHolder holder = (ViewHolder) ((AdapterContextMenuInfo) menuInfo).targetView.getTag();
        menu.setHeaderTitle(holder.nameView.getText());
        if (holder.icon != null) {
            setHeaderIcon(menu, holder.icon);
        }
        menu.add(Menu.NONE, R.string.app_info, Menu.NONE, R.string.app_info);
        if (mActivity.getPreventPackages().containsKey(holder.packageName)) {
            menu.add(Menu.NONE, R.string.remove, Menu.NONE, R.string.remove);
        } else {
            menu.add(Menu.NONE, R.string.prevent, Menu.NONE, R.string.prevent);
        }
        if (getMainIntent(holder.packageName) != null) {
            menu.add(Menu.NONE, R.string.open, Menu.NONE, R.string.open);
        }
        if (holder.canUninstall) {
            menu.add(Menu.NONE, R.string.uninstall, Menu.NONE, R.string.uninstall);
        }
    }

    private void setHeaderIcon(ContextMenu menu, Drawable icon) {
        int width = getHeaderIconWidth();
        if (icon.getMinimumWidth() <= width) {
            menu.setHeaderIcon(icon);
        } else if (icon instanceof BitmapDrawable) {
            Bitmap bitmap = Bitmap.createScaledBitmap(((BitmapDrawable) icon).getBitmap(), width, width, false);
            menu.setHeaderIcon(new BitmapDrawable(getResources(), bitmap));
        }
    }

    @Override
    public boolean onContextItemSelected(MenuItem item) {
        if (mActivity == null || item == null) {
            return false;
        }
        ViewHolder holder = (ViewHolder) ((AdapterContextMenuInfo) item.getMenuInfo()).targetView.getTag();
        return onContextItemSelected(holder, holder.packageName, item.getItemId());
    }

    private boolean onContextItemSelected(ViewHolder holder, String packageName, int id) {
        switch (id) {
            case R.string.app_info:
            case R.string.uninstall:
                return startActivity(id, packageName);
            case R.string.open:
                return startPackage(packageName);
            case R.string.remove:
            case R.string.prevent:
                return updatePrevent(id, holder, packageName);
            default:
                return false;
        }
    }

    private boolean startActivity(int id, String packageName) {
        String action;
        if (id == R.string.app_info) {
            action = android.provider.Settings.ACTION_APPLICATION_DETAILS_SETTINGS;
        } else if (id == R.string.uninstall) {
            action = Intent.ACTION_DELETE;
        } else {
            return false;
        }
        mActivity.startActivity(new Intent(action, Uri.fromParts("package", packageName, null)));
        return true;
    }

    private boolean startPackage(String packageName) {
        Intent intent = getMainIntent(packageName);
        if (intent != null) {
            mActivity.startActivity(intent);
        }
        return true;
    }

    private boolean updatePrevent(int id, ViewHolder holder, String packageName) {
        if (id == R.string.prevent) {
            holder.preventView.setVisibility(View.VISIBLE);
            holder.preventView.setImageResource(holder.running != null ? R.drawable.ic_menu_stop : R.drawable.ic_menu_block);
            mActivity.changePrevent(packageName, true);
        } else if (id == R.string.remove) {
            holder.preventView.setVisibility(View.GONE);
            mActivity.changePrevent(packageName, false);
        }
        return true;
    }

    private Intent getMainIntent(String packageName) {
        return mActivity.getPackageManager().getLaunchIntentForPackage(packageName);
    }

    public void refresh(boolean force) {
        if (mActivity != null) {
            setNewAdapterIfNeeded(mActivity, force);
            if (mActivity.getSelection().isEmpty()) {
                check.setChecked(false);
            }
        }
    }

    protected abstract Set<String> getPackageNames(PreventActivity activity);

    protected abstract int getQueryHint();

    protected abstract String getDefaultQuery();

    public void saveListPosition() {
        if (mAdapter != null) {
            ListView l = getListView();
            int position = l.getFirstVisiblePosition();
            View v = l.getChildAt(0);
            int top = (v == null) ? 0 : v.getTop();
            setListPosition(new Position(position, top));
        }
    }

    private void setListPosition(Position position) {
        positions.put(getClass().getName(), position);
    }

    private Position getListPosition() {
        return positions.get(getClass().getName());
    }

    private void setNewAdapterIfNeeded(PreventActivity activity, boolean force) {
        Set<String> names;
        if (force || prevNames == null) {
            names = getPackageNames(activity);
        } else {
            names = prevNames;
        }
        if (mAdapter == null || !names.equals(prevNames)) {
            if (mAdapter != null) {
                setListAdapter(null);
            }
            mAdapter = new Adapter(activity, names, filter);
            setListAdapter(mAdapter);
            if (prevNames == null) {
                prevNames = new HashSet<String>();
            }
            prevNames.clear();
            prevNames.addAll(names);
        } else {
            mAdapter.notifyDataSetChanged();
            Position position = getListPosition();
            if (position != null) {
                getListView().setSelectionFromTop(position.pos, position.top);
            }
        }
    }

    public void startTaskIfNeeded() {
        mAdapter.startTaskIfNeeded();
    }

    private static class Position {
        int pos;
        int top;

        public Position(int pos, int top) {
            this.pos = pos;
            this.top = top;
        }
    }

    private static class AppInfo implements Comparable<AppInfo> {
        int flags;
        String name = "";
        String packageName;
        Set<Integer> running;

        public AppInfo(String packageName, String name, Set<Integer> running) {
            super();
            this.packageName = packageName;
            if (name != null) {
                this.name = name;
            }
            this.running = running;
        }

        public AppInfo setFlags(int flags) {
            this.flags = flags;
            return this;
        }

        public boolean isSystem() {
            return PackageUtils.isSystemPackage(this.flags);
        }

        @Override
        public String toString() {
            return (running == null ? "1" : "0") + (isSystem() ? "1" : "0") + "/" + name + "/" + packageName;
        }

        @Override
        public int compareTo(AppInfo another) {
            return Collator.getInstance().compare(toString(), another.toString());
        }

        @Override
        public boolean equals(Object obj) {
            return obj instanceof AppInfo && compareTo((AppInfo) obj) == 0;
        }

        @Override
        public int hashCode() {
            return toString().hashCode();
        }
    }

    private static class ViewHolder {
        String label;
        String packageName;
        CheckBox checkView;
        ImageView iconView;
        TextView nameView;
        TextView summaryView;
        TextView loadingView;
        ImageView preventView;
        Drawable icon;
        Set<Integer> running;
        RetrieveIconTask task;
        boolean canUninstall;
    }

    private class Adapter extends ArrayAdapter<AppInfo> {
        private PackageManager pm;
        private LayoutInflater inflater;
        private PreventActivity mActivity;
        private final CompoundButton.OnCheckedChangeListener mListener;

        private List<AppInfo> mAppInfos = new ArrayList<AppInfo>();
        private Set<String> mNames = new HashSet<String>();
        private Set<String> mFiltered;
        private Filter mFilter;
        private View mView;
        private RetrieveInfoTask mTask;

        public Adapter(PreventActivity activity) {
            super(activity, R.layout.item);
            mActivity = activity;
            pm = mActivity.getPackageManager();
            inflater = LayoutInflater.from(activity);
            mListener = new CompoundButton.OnCheckedChangeListener() {
                @Override
                public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
                    ViewHolder holder = (ViewHolder) buttonView.getTag();
                    Set<String> selections = mActivity.getSelection();
                    if (isChecked) {
                        selections.add(holder.packageName);
                    } else {
                        selections.remove(holder.packageName);
                    }
                    mActivity.checkSelection();
                }
            };
        }

        public Adapter(final PreventActivity activity, Set<String> names, View view) {
            this(activity);
            mView = view;
            mNames.addAll(names);
            mTask = new RetrieveInfoTask();
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent) {
            View view = convertView;
            if (view == null) {
                view = inflater.inflate(R.layout.item, null, true);
                ViewHolder viewHolder = new ViewHolder();
                viewHolder.checkView = (CheckBox) view.findViewById(R.id.check);
                viewHolder.iconView = (ImageView) view.findViewById(R.id.icon);
                viewHolder.nameView = (TextView) view.findViewById(R.id.name);
                viewHolder.summaryView = (TextView) view.findViewById(R.id.summary);
                viewHolder.loadingView = (TextView) view.findViewById(R.id.loading);
                viewHolder.preventView = (ImageView) view.findViewById(R.id.prevent);
                viewHolder.checkView.setOnCheckedChangeListener(mListener);
                viewHolder.checkView.setTag(viewHolder);
                view.setTag(viewHolder);
            }

            ViewHolder holder = (ViewHolder) view.getTag();
            AppInfo appInfo = getItem(position);
            holder.label = appInfo.name;
            holder.packageName = appInfo.packageName;
            holder.nameView.setText(appInfo.name);
            holder.summaryView.setVisibility(View.GONE);
            holder.loadingView.setVisibility(View.VISIBLE);
            holder.checkView.setChecked(mActivity.getSelection().contains(holder.packageName));
            Boolean result = mActivity.getPreventPackages().get(appInfo.packageName);
            if (appInfo.isSystem()) {
                view.setBackgroundColor(mActivity.getDangerousColor());
            } else {
                view.setBackgroundColor(mActivity.getTransparentColor());
            }
            holder.canUninstall = ((appInfo.flags & ApplicationInfo.FLAG_SYSTEM) == 0) || ((appInfo.flags & ApplicationInfo.FLAG_UPDATED_SYSTEM_APP) != 0);
            if (result == null) {
                holder.preventView.setVisibility(View.INVISIBLE);
            } else {
                holder.preventView.setVisibility(View.VISIBLE);
                holder.preventView.setImageResource(result ? R.drawable.ic_menu_block : R.drawable.ic_menu_stop);
            }
            if (holder.task != null) {
                holder.task.cancel(true);
            }
            holder.task = new RetrieveIconTask();
            holder.task.execute(holder, appInfo);
            return view;
        }


        public Filter getFilter() {
            if (mFilter == null) {
                mFilter = new SimpleFilter();
            }
            return mFilter;
        }

        public Collection<String> getAllPackages() {
            if (mFiltered == null) {
                return mNames;
            } else {
                return mFiltered;
            }
        }

        public void startTaskIfNeeded() {
            AsyncTask.Status status = mTask.getStatus();
            if (status == AsyncTask.Status.PENDING) {
                mTask.execute();
            } else if (mTask.dialog != null) {
                mTask.dialog.show();
            }
        }

        private class SimpleFilter extends Filter {
            @Override
            protected FilterResults performFiltering(CharSequence prefix) {
                FilterResults results = new FilterResults();
                String query = null;
                if (prefix != null && prefix.length() > 0) {
                    query = prefix.toString().toLowerCase(Locale.US);
                }
                if (mFiltered == null) {
                    mFiltered = new HashSet<String>();
                }
                List<AppInfo> values = new ArrayList<AppInfo>();
                if (query == null) {
                    values.addAll(mAppInfos);
                    mFiltered.addAll(mNames);
                } else {
                    mFiltered.clear();
                    for (AppInfo appInfo : mAppInfos) {
                        if (match(query, appInfo)) {
                            values.add(appInfo);
                            mFiltered.add(appInfo.packageName);
                        }
                    }
                }
                results.values = values;
                results.count = values.size();
                return results;
            }

            @Override
            @SuppressWarnings("unchecked")
            protected void publishResults(CharSequence constraint, FilterResults results) {
                setNotifyOnChange(false);
                clear();
                for (AppInfo appInfo : (List<AppInfo>) results.values) {
                    add(appInfo);
                }
                notifyDataSetChanged();
            }

            private boolean match(String query, AppInfo appInfo) {
                return contains(query, appInfo)
                        || queryForThirdParty(query, appInfo)
                        || queryExtra(query, appInfo)
                        || queryCombined(query, appInfo);
            }

            private boolean queryCombined(String query, AppInfo appInfo) {
                if ("-sg".equals(query)) {
                    return appInfo.isSystem() && !GmsUtils.isGapps(mActivity.getPackageManager(), appInfo.packageName);
                } else if ("-3g".equals(query)) {
                    return !appInfo.isSystem() || GmsUtils.isGapps(mActivity.getPackageManager(), appInfo.packageName);
                }
                return false;
            }

            private boolean queryExtra(String query, AppInfo appInfo) {
                return queryForSystem(query, appInfo)
                        || queryForGapps(query, appInfo)
                        || queryForRunning(query, appInfo)
                        || queryForEnabled(query, appInfo);
            }

            private boolean contains(String query, AppInfo appInfo) {
                return appInfo.name.toLowerCase(Locale.US).contains(query);
            }

            private boolean queryForThirdParty(String query, AppInfo appInfo) {
                return "-3".equals(query) && !appInfo.isSystem();
            }

            private boolean queryForSystem(String query, AppInfo appInfo) {
                return "-s".equals(query) && appInfo.isSystem();
            }

            private boolean queryForGapps(String query, AppInfo appInfo) {
                return "-g".equals(query) && GmsUtils.isGapps(mActivity.getPackageManager(), appInfo.packageName);
            }

            private boolean queryForRunning(String query, AppInfo appInfo) {
                return "-r".equals(query) && mActivity.getRunningProcesses().containsKey(appInfo.packageName);
            }

            private boolean queryForEnabled(String query, AppInfo appInfo) {
                return "-e".equals(query) && !mActivity.getPreventPackages().containsKey(appInfo.packageName);
            }
        }

        private class RetrieveInfoTask extends AsyncTask<Void, Integer, Set<AppInfo>> {
            ProgressDialog dialog;

            @Override
            protected void onPreExecute() {
                dialog = new ProgressDialog(mActivity);
                dialog.setTitle(R.string.app_name);
                dialog.setIcon(R.drawable.ic_launcher);
                dialog.setProgressStyle(ProgressDialog.STYLE_HORIZONTAL);
                dialog.setCancelable(false);
                dialog.setMax(mNames.size());
                dialog.show();
            }

            @Override
            protected Set<AppInfo> doInBackground(Void... params) {
                Map<String, Set<Integer>> running = mActivity.getRunningProcesses();
                Set<AppInfo> applications = new TreeSet<AppInfo>();
                int i = 1;
                for (String name : mNames) {
                    publishProgress(++i);
                    ApplicationInfo info;
                    try {
                        info = pm.getApplicationInfo(name, 0);
                    } catch (NameNotFoundException e) { // NOSONAR
                        info = null;
                    }
                    if (info == null || !info.enabled) {
                        continue;
                    }
                    String label = info.loadLabel(pm).toString();
                    applications.add(new AppInfo(name, label, running.get(name)).setFlags(info.flags));
                }
                return applications;
            }

            @Override
            protected void onProgressUpdate(Integer... progress) {
                if (dialog != null) {
                    dialog.setProgress(progress[0]);
                }
            }

            @Override
            protected void onPostExecute(Set<AppInfo> applications) {
                for (AppInfo application : applications) {
                    add(application);
                    mAppInfos.add(application);
                }
                if (dialog != null) {
                    dialog.dismiss();
                    dialog = null;
                }
                if (mView != null) {
                    mView.setVisibility(View.VISIBLE);
                }
                String query = search.getText().toString();
                if (TextUtils.isEmpty(query)) {
                    query = getDefaultQuery();
                }
                if (!TextUtils.isEmpty(query)) {
                    getFilter().filter(query);
                }
            }
        }

    }

    private class RetrieveIconTask extends AsyncTask<Object, Void, ViewHolder> {
        final PackageManager pm = mActivity.getPackageManager();

        @Override
        protected ViewHolder doInBackground(Object... params) {
            ViewHolder holder = (ViewHolder) params[0];
            AppInfo appInfo = (AppInfo) params[1];
            try {
                holder.icon = pm.getApplicationIcon(appInfo.packageName);
            } catch (NameNotFoundException e) { // NOSONAR
                // do nothing
            }
            holder.running = mActivity.getRunningProcesses().get(appInfo.packageName);
            return holder;
        }

        @Override
        protected void onPostExecute(ViewHolder holder) {
            holder.iconView.setImageDrawable(holder.icon);
            holder.loadingView.setVisibility(View.GONE);
            holder.summaryView.setText(formatRunning(holder.running));
            holder.summaryView.setVisibility(View.VISIBLE);
        }

        private CharSequence formatRunning(Set<Integer> running) {
            if (running == null) {
                return mActivity.getString(R.string.notrunning);
            } else {
                if (running.contains(RunningAppProcessInfo.IMPORTANCE_SERVICE) && running.contains(-RunningAppProcessInfo.IMPORTANCE_SERVICE)) {
                    running.remove(-RunningAppProcessInfo.IMPORTANCE_SERVICE);
                }
                return doFormatRunning(running);
            }
        }

        private CharSequence doFormatRunning(Set<Integer> running) {
            Set<String> sets = new TreeSet<String>();
            for (Integer i : running) {
                switch (i) {
                    case RunningAppProcessInfo.IMPORTANCE_BACKGROUND:
                        sets.add(mActivity.getString(R.string.background));
                        break;
                    case RunningAppProcessInfo.IMPORTANCE_EMPTY:
                        sets.add(mActivity.getString(R.string.empty));
                        break;
                    case RunningAppProcessInfo.IMPORTANCE_FOREGROUND:
                        sets.add(mActivity.getString(R.string.foreground));
                        break;
                    case RunningAppProcessInfo.IMPORTANCE_PERCEPTIBLE:
                        sets.add(mActivity.getString(R.string.perceptible));
                        break;
                    case RunningAppProcessInfo.IMPORTANCE_SERVICE:
                        sets.add(mActivity.getString(R.string.service));
                        break;
                    case RunningAppProcessInfo.IMPORTANCE_VISIBLE:
                        sets.add(mActivity.getString(R.string.visible));
                        break;
                    case -RunningAppProcessInfo.IMPORTANCE_SERVICE:
                        sets.add(mActivity.getString(R.string.service) + "(" + mActivity.getString(R.string.notstarted) + ")");
                        break;
                    default:
                        break;
                }
            }
            return toString(sets);
        }

        private CharSequence toString(Set<String> sets) {
            StringBuilder buffer = new StringBuilder();
            Iterator<?> it = sets.iterator();
            while (it.hasNext()) {
                buffer.append(it.next());
                if (it.hasNext()) {
                    buffer.append(", ");
                } else {
                    break;
                }
            }
            return buffer.toString();
        }
    }

    public static class Applications extends PreventFragment {

        @Override
        protected Set<String> getPackageNames(PreventActivity activity) {
            Set<String> names = new HashSet<String>();
            PackageManager pm = activity.getPackageManager();
            for (PackageInfo pkgInfo : pm.getInstalledPackages(0)) {
                ApplicationInfo appInfo = pkgInfo.applicationInfo;
                if (PackageUtils.canPrevent(pm, appInfo)) {
                    names.add(appInfo.packageName);
                }
            }
            return names;
        }

        @Override
        protected int getQueryHint() {
            return R.string.query_hint_system;
        }

        @Override
        protected String getDefaultQuery() {
            return "-3g";
        }

    }

    public static class PreventList extends PreventFragment {

        @Override
        protected Set<String> getPackageNames(PreventActivity activity) {
            Set<String> names = new HashSet<String>();
            PackageManager pm = activity.getPackageManager();
            Set<String> removes = new HashSet<String>();
            for (String packageName : activity.getPreventPackages().keySet()) {
                ApplicationInfo appInfo;
                try {
                    appInfo = pm.getApplicationInfo(packageName, 0);
                    if (PackageUtils.canPrevent(pm, appInfo)) {
                        names.add(packageName);
                        continue;
                    }
                } catch (PackageManager.NameNotFoundException e) { // NOSONAR
                    // do nothing
                }
                removes.add(packageName);
            }
            if (!removes.isEmpty()) {
                PreventUtils.update(getActivity(), removes.toArray(new String[removes.size()]), false);
            }
            return names;
        }

        @Override
        protected int getQueryHint() {
            return R.string.query_hint;
        }

        @Override
        protected String getDefaultQuery() {
            return null;
        }

    }

}